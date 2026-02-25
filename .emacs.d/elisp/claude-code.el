;;; claude-code.el --- Emacs interface to Claude Code CLI -*- lexical-binding: t -*-

;;; Commentary:
;; A chat interface that wraps the Claude Code CLI in print mode,
;; providing a magit-inspired buffer with collapsible tool call sections,
;; session management, and file navigation.
;;
;; Each user message spawns `claude -p --output-format stream-json`
;; with `--resume` for conversation continuity.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'ring)
(require 'diff-mode)
(require 'transient)

;;;; Customization

(defgroup claude-code nil
  "Emacs interface to Claude Code CLI."
  :group 'tools
  :prefix "claude-code-")

(defcustom claude-code-executable "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-always-allowed-tools
  '("Read" "Grep" "Glob")
  "Tool names permanently approved via --allowedTools.
These are always passed to the CLI. For session-only approvals,
use the tools transient (\\[nj/claude-code-tools-transient])."
  :type '(repeat string)
  :group 'claude-code)

(defconst claude-code--known-tools
  '("Read" "Grep" "Glob" "Edit" "Write" "Bash"
    "WebFetch" "WebSearch" "NotebookEdit" "TodoWrite")
  "Known Claude Code tool names.")

(defcustom claude-code-extra-args nil
  "Additional CLI arguments to pass to every invocation."
  :type '(repeat string)
  :group 'claude-code)

;;;; Faces

(defgroup claude-code-faces nil
  "Faces for claude-code chat buffers."
  :group 'claude-code)

(defface claude-code-prompt-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the input prompt."
  :group 'claude-code-faces)

(defface claude-code-user-input-face
  '((t :inherit default :weight bold))
  "Face for echoed user input."
  :group 'claude-code-faces)

(defface claude-code-assistant-face
  '((t :inherit default))
  "Face for assistant response text."
  :group 'claude-code-faces)

(defface claude-code-section-header-face
  '((((class color) (background dark))
     :background "#3a3a3a" :extend t :weight bold)
    (((class color) (background light))
     :background "#e0e0e0" :extend t :weight bold))
  "Face for collapsible tool section headers."
  :group 'claude-code-faces)

(defface claude-code-section-success-face
  '((t :inherit success :weight bold))
  "Face for the success indicator in section headers."
  :group 'claude-code-faces)

(defface claude-code-section-error-face
  '((t :inherit error :weight bold))
  "Face for the error indicator in section headers."
  :group 'claude-code-faces)

(defface claude-code-file-link-face
  '((t :inherit link))
  "Face for clickable file path links."
  :group 'claude-code-faces)

(defface claude-code-cost-face
  '((t :inherit font-lock-comment-face))
  "Face for cost/usage display."
  :group 'claude-code-faces)

(defface claude-code-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for turn separators."
  :group 'claude-code-faces)

;;;; Buffer-local variables

(defvar-local claude-code--session-id nil
  "Session ID for the current conversation.")

(defvar-local claude-code--project-root nil
  "Project root directory for this session.")

(defvar-local claude-code--status 'idle
  "Current status: idle, streaming, or error.")

(defvar-local claude-code--process nil
  "Current active subprocess.")

(defvar-local claude-code--input-start-marker nil
  "Marker at the beginning of the editable input area.")

(defvar-local claude-code--output-end-marker nil
  "Marker at the end of the read-only output area.")

(defvar-local claude-code--parse-buffer ""
  "Incomplete line buffer for NDJSON process filter.")

(defvar-local claude-code--input-history nil
  "Ring of previous inputs.")

(defvar-local claude-code--input-history-index -1
  "Current position in the input history ring.")

(defvar-local claude-code--input-history-saved nil
  "Saved current input before history navigation.")

(defvar-local claude-code--pending-tool-uses nil
  "Alist mapping tool_use_id to (name . input) for correlating tool results.")

(defvar-local claude-code--session-allowed-tools nil
  "Tool names approved for this session only.
Merged with `claude-code-always-allowed-tools' when building commands.")

(defvar-local claude-code--total-cost 0.0
  "Accumulated cost in USD for this session.")

;;;; Global variables

(defvar claude-code--sessions nil
  "Alist of (session-id . buffer) for all active sessions.")

(defvar claude-code--history-size 100
  "Maximum number of input history entries per session.")

;;;; Hooks

(defvar claude-code-after-tool-hook nil
  "Hook run after a tool completes.
Called with (TOOL-NAME TOOL-INPUT RESULT).")

;;;; Mode line

(defun claude-code--mode-line-status ()
  "Return mode-line string indicating Claude status."
  (pcase claude-code--status
    ('idle      (propertize " [Claude:idle]" 'face 'success))
    ('streaming (propertize " [Claude:streaming]" 'face 'warning))
    ('error     (propertize " [Claude:error]" 'face 'error))
    (_          "")))

;;;; Major mode

(defvar claude-code-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code--send-input)
    (define-key map (kbd "M-p") #'claude-code--history-prev)
    (define-key map (kbd "M-n") #'claude-code--history-next)
    (define-key map (kbd "TAB") #'claude-code--toggle-section)
    (define-key map (kbd "C-g") #'claude-code--interrupt-or-quit)
    (define-key map (kbd "C-c C-l") #'nj/claude-code-transient)
    (define-key map (kbd "C-c C-c") #'claude-code--kill-process)
    map)
  "Keymap for `claude-code-mode'.")

(define-derived-mode claude-code-mode special-mode "Claude"
  "Major mode for interacting with Claude Code CLI.

\\{claude-code-mode-map}"
  :group 'claude-code
  (setq-local claude-code--input-history (make-ring claude-code--history-size))
  (setq-local claude-code--parse-buffer "")
  (setq-local claude-code--pending-tool-uses nil)
  (setq-local claude-code--session-id nil)
  (setq-local claude-code--status 'idle)
  (setq-local claude-code--total-cost 0.0)
  (setq-local claude-code--session-allowed-tools nil)
  (setq-local claude-code--output-end-marker (point-min-marker))
  (setq-local claude-code--input-start-marker (point-min-marker))
  (setq buffer-read-only nil)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq mode-line-process '(:eval (claude-code--mode-line-status)))
  (add-hook 'claude-code-after-tool-hook #'claude-code--maybe-revert-buffers nil t))

;;;; Buffer initialization

(defun claude-code--init-buffer (buffer project-root)
  "Initialize BUFFER as a claude-code chat buffer for PROJECT-ROOT."
  (with-current-buffer buffer
    (claude-code-mode)
    (setq claude-code--project-root project-root)
    (setq default-directory project-root)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "Claude Code — %s\n\n"
                                  (abbreviate-file-name project-root))
                          'face 'claude-code-cost-face
                          'read-only t))
      (set-marker claude-code--output-end-marker (point)))
    (claude-code--insert-prompt)
    (goto-char (point-max))))

(defun claude-code--insert-prompt ()
  "Insert the input prompt at the end of the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    ;; Make everything above read-only
    (add-text-properties (point-min) (point)
                         '(read-only t front-sticky (read-only)))
    ;; Insert prompt (read-only)
    (insert (propertize "Claude> "
                        'face 'claude-code-prompt-face
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky '(read-only face)))
    (set-marker claude-code--input-start-marker (point))))

;;;; Input handling

(defun claude-code--get-input ()
  "Return the current input text."
  (buffer-substring-no-properties claude-code--input-start-marker (point-max)))

(defun claude-code--send-input ()
  "Send the current input to Claude."
  (interactive)
  (when (eq claude-code--status 'streaming)
    (user-error "Claude is still responding — use C-g to interrupt"))
  (let ((input (string-trim (claude-code--get-input))))
    (when (string-empty-p input)
      (user-error "Empty input"))
    (ring-insert claude-code--input-history input)
    (setq claude-code--input-history-index -1)
    (claude-code--append-user-input input)
    (claude-code--clear-input)
    (claude-code--spawn-process input)))

(defun claude-code--append-user-input (input)
  "Append echoed user INPUT to the output area."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char claude-code--output-end-marker)
      (insert (propertize (concat "You: " input "\n\n")
                          'face 'claude-code-user-input-face
                          'read-only t))
      (set-marker claude-code--output-end-marker (point)))))

(defun claude-code--clear-input ()
  "Clear the input area."
  (let ((inhibit-read-only t))
    (delete-region claude-code--input-start-marker (point-max))
    (goto-char (point-max))))

(defun claude-code--replace-input (text)
  "Replace the current input with TEXT."
  (let ((inhibit-read-only t))
    (delete-region claude-code--input-start-marker (point-max))
    (goto-char claude-code--input-start-marker)
    (insert text)))

;;;; Input history

(defun claude-code--history-prev ()
  "Navigate to the previous input in history."
  (interactive)
  (when (ring-empty-p claude-code--input-history)
    (user-error "No input history"))
  (when (= claude-code--input-history-index -1)
    (setq claude-code--input-history-saved (claude-code--get-input)))
  (let ((max-idx (1- (ring-length claude-code--input-history))))
    (when (< claude-code--input-history-index max-idx)
      (cl-incf claude-code--input-history-index)
      (claude-code--replace-input
       (ring-ref claude-code--input-history claude-code--input-history-index)))))

(defun claude-code--history-next ()
  "Navigate to the next input in history."
  (interactive)
  (cond
   ((> claude-code--input-history-index 0)
    (cl-decf claude-code--input-history-index)
    (claude-code--replace-input
     (ring-ref claude-code--input-history claude-code--input-history-index)))
   ((= claude-code--input-history-index 0)
    (setq claude-code--input-history-index -1)
    (claude-code--replace-input (or claude-code--input-history-saved "")))))

;;;; Process management

(defun claude-code--effective-allowed-tools ()
  "Return the merged list of allowed tools (permanent + session)."
  (cl-remove-duplicates
   (append claude-code-always-allowed-tools
           claude-code--session-allowed-tools)
   :test #'string=))

(defun claude-code--build-command (message)
  "Build the CLI argument list for sending MESSAGE."
  (let ((args (list claude-code-executable
                    "-p" message
                    "--output-format" "stream-json"
                    "--verbose"))
        (tools (claude-code--effective-allowed-tools)))
    (when claude-code--session-id
      (setq args (append args (list "--resume" claude-code--session-id))))
    (when tools
      (setq args (append args (list "--allowedTools"
                                    (string-join tools ",")))))
    (when claude-code-extra-args
      (setq args (append args claude-code-extra-args)))
    args))

(defun claude-code--spawn-process (message)
  "Spawn a claude subprocess for MESSAGE."
  (setq claude-code--status 'streaming)
  (setq claude-code--parse-buffer "")
  (setq claude-code--pending-tool-uses nil)
  (force-mode-line-update)
  (let* ((args (claude-code--build-command message))
         (process-environment (cons "CLAUDECODE="
                                    process-environment))
         (proc (make-process
                :name (format "claude-code<%s>"
                              (or claude-code--session-id "new"))
                :buffer (current-buffer)
                :command args
                :connection-type 'pipe
                :filter #'claude-code--process-filter
                :sentinel #'claude-code--process-sentinel
                :noquery t)))
    (setq claude-code--process proc)
    (set-process-query-on-exit-flag proc nil)))

(defun claude-code--process-sentinel (process event)
  "Handle PROCESS completion EVENT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq claude-code--process nil)
      ;; Process remaining data
      (when (not (string-empty-p claude-code--parse-buffer))
        (claude-code--handle-line claude-code--parse-buffer)
        (setq claude-code--parse-buffer ""))
      (let ((exit-status (process-exit-status process)))
        (if (zerop exit-status)
            (setq claude-code--status 'idle)
          (setq claude-code--status 'error)
          (claude-code--append-output
           (propertize (format "\n[Process exited with code %d]\n" exit-status)
                       'face 'claude-code-section-error-face))))
      (force-mode-line-update)
      (claude-code--finalize-turn))))

(defun claude-code--finalize-turn ()
  "Insert separator after a turn completes and refresh prompt."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char claude-code--output-end-marker)
      (insert (propertize (concat "\n" (make-string 60 ?─) "\n\n")
                          'face 'claude-code-separator-face
                          'read-only t))
      (set-marker claude-code--output-end-marker (point))))
  ;; Smart scroll: only if point is near the bottom
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (when (>= (point) claude-code--input-start-marker)
        (goto-char (point-max))))))

(defun claude-code--kill-process ()
  "Kill the current claude process."
  (interactive)
  (when (and claude-code--process (process-live-p claude-code--process))
    (kill-process claude-code--process)
    (message "Claude process killed")))

(defun claude-code--interrupt-or-quit ()
  "Send SIGINT if streaming, otherwise normal `keyboard-quit'."
  (interactive)
  (if (and claude-code--process
           (eq claude-code--status 'streaming)
           (process-live-p claude-code--process))
      (progn
        (interrupt-process claude-code--process)
        (message "Sent interrupt to Claude"))
    (keyboard-quit)))

;;;; NDJSON parsing

(defun claude-code--process-filter (process output)
  "Accumulate OUTPUT from PROCESS and handle complete NDJSON lines."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq claude-code--parse-buffer
            (concat claude-code--parse-buffer output))
      (let ((lines (split-string claude-code--parse-buffer "\n")))
        (setq claude-code--parse-buffer (car (last lines)))
        (dolist (line (butlast lines))
          (unless (string-empty-p (string-trim line))
            (claude-code--handle-line (string-trim line))))))))

(defun claude-code--handle-line (line)
  "Parse a single NDJSON LINE and dispatch by type."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (data (json-read-from-string line))
             (msg-type (alist-get 'type data)))
        (pcase msg-type
          ("system"    (claude-code--handle-system data))
          ("assistant" (claude-code--handle-assistant data))
          ("user"      (claude-code--handle-user data))
          ("result"    (claude-code--handle-result data))))
    (json-error
     (message "claude-code: JSON parse error: %s"
              (substring line 0 (min 80 (length line)))))))

;;;; Event handlers

(defun claude-code--handle-system (data)
  "Handle system message DATA, extracting session_id."
  (let ((session-id (alist-get 'session_id data)))
    (when session-id
      (setq claude-code--session-id session-id)
      (claude-code--register-session session-id (current-buffer))
      (claude-code--append-output
       (propertize (format "[Session: %s]\n"
                           (substring session-id 0 (min 8 (length session-id))))
                   'face 'claude-code-cost-face)))))

(defun claude-code--handle-assistant (data)
  "Handle assistant message DATA — render text and tool_use blocks."
  (let* ((message (alist-get 'message data))
         (content (alist-get 'content message)))
    (dolist (block content)
      (let ((block-type (alist-get 'type block)))
        (pcase block-type
          ("text"
           (claude-code--append-output
            (propertize (claude-code--linkify-paths (alist-get 'text block))
                        'face 'claude-code-assistant-face)))
          ("tool_use"
           (let ((tool-id (alist-get 'id block))
                 (tool-name (alist-get 'name block))
                 (tool-input (alist-get 'input block)))
             (push (cons tool-id (cons tool-name tool-input))
                   claude-code--pending-tool-uses)
             (claude-code--render-tool-use-section tool-name tool-input tool-id))))))))

(defun claude-code--handle-user (data)
  "Handle user message DATA (tool results)."
  (let* ((message (alist-get 'message data))
         (content (alist-get 'content message)))
    (dolist (block content)
      (when (equal (alist-get 'type block) "tool_result")
        (let* ((tool-use-id (alist-get 'tool_use_id block))
               (result-content (alist-get 'content block))
               (is-error (alist-get 'is_error block))
               (pending (assoc tool-use-id claude-code--pending-tool-uses))
               (result-text (if (stringp result-content)
                                result-content
                              (json-encode result-content))))
          (when pending
            (claude-code--update-tool-section tool-use-id result-text is-error)
            (let ((tool-name (cadr pending))
                  (tool-input (cddr pending)))
              (run-hook-with-args 'claude-code-after-tool-hook
                                  tool-name tool-input result-text))))))))

(defun claude-code--handle-result (data)
  "Handle result message DATA — display cost and status."
  (let ((cost (alist-get 'total_cost_usd data))
        (turns (alist-get 'num_turns data))
        (duration (alist-get 'duration_ms data))
        (is-error (alist-get 'is_error data)))
    (when cost
      (setq claude-code--total-cost cost))
    (claude-code--append-output
     (propertize (format "\n[Cost: $%.4f | Turns: %s | %sms]\n"
                         (or cost 0)
                         (or turns "?")
                         (or duration "?"))
                 'face 'claude-code-cost-face))
    (when is-error
      (setq claude-code--status 'error))))

;;;; Output rendering

(defun claude-code--append-output (text)
  "Append TEXT to the output area before the prompt."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char claude-code--output-end-marker)
      (insert (propertize text 'read-only t))
      (set-marker claude-code--output-end-marker (point)))
    ;; Smart scroll: follow output only if point is at the input area
    (when-let ((win (get-buffer-window (current-buffer))))
      (with-selected-window win
        (when (>= (point) claude-code--input-start-marker)
          (goto-char (point-max)))))))

;;;; Tool sections — magit-style collapsible

(defun claude-code--tool-summary (tool-name tool-input)
  "Generate a one-line summary for TOOL-NAME given TOOL-INPUT."
  (pcase tool-name
    ((or "Edit" "Write")
     (format "%s  %s" tool-name
             (or (alist-get 'file_path tool-input) "unknown")))
    ("Read"
     (format "Read  %s" (or (alist-get 'file_path tool-input) "unknown")))
    ("Bash"
     (let* ((cmd (or (alist-get 'command tool-input) ""))
            (short (if (> (length cmd) 50)
                       (concat (substring cmd 0 47) "...")
                     cmd)))
       (format "Bash  %s" short)))
    ("Grep"
     (format "Grep  \"%s\" in %s"
             (or (alist-get 'pattern tool-input) "?")
             (or (alist-get 'path tool-input) ".")))
    ("Glob"
     (format "Glob  %s" (or (alist-get 'pattern tool-input) "?")))
    (_
     (format "%s" tool-name))))

(defun claude-code--render-tool-use-section (tool-name tool-input tool-id)
  "Render a collapsible section for TOOL-NAME with TOOL-INPUT and TOOL-ID."
  (let ((inhibit-read-only t)
        (summary (claude-code--tool-summary tool-name tool-input))
        body-start)
    (save-excursion
      (goto-char claude-code--output-end-marker)
      ;; Section header
      (insert (propertize (format "\n▶ %s  ⏳\n" summary)
                          'face 'claude-code-section-header-face
                          'claude-code-section-id tool-id
                          'claude-code-section-type 'header
                          'read-only t))
      ;; Body (starts empty, will be populated by tool result)
      (setq body-start (point))
      (insert (propertize "" 'read-only t))
      ;; Create invisible overlay for body
      (let ((ov (make-overlay body-start (point) nil t nil)))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'claude-code-section-id tool-id)
        (overlay-put ov 'claude-code-section-type 'body)
        (overlay-put ov 'isearch-open-invisible #'claude-code--isearch-reveal))
      (set-marker claude-code--output-end-marker (point)))))

(defun claude-code--update-tool-section (tool-id result-text is-error)
  "Update the section for TOOL-ID with RESULT-TEXT."
  (let ((inhibit-read-only t))
    ;; Update header status indicator
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (equal (get-text-property (point) 'claude-code-section-id) tool-id)
                   (eq (get-text-property (point) 'claude-code-section-type) 'header))
          (let ((line-end (line-end-position)))
            (when (search-forward "⏳" line-end t)
              (replace-match (if is-error "✗" "✓"))
              ;; Update header arrow to indicate expandable
              (beginning-of-line)
              (when (search-forward "▶" (line-end-position) t)
                (replace-match "▸")))))
        (forward-line 1)))
    ;; Populate body overlay
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (equal (overlay-get ov 'claude-code-section-id) tool-id)
                 (eq (overlay-get ov 'claude-code-section-type) 'body))
        (let ((pending (assoc tool-id claude-code--pending-tool-uses)))
          (when pending
            (save-excursion
              (goto-char (overlay-start ov))
              (let* ((tool-name (cadr pending))
                     (tool-input (cddr pending))
                     (formatted (claude-code--format-tool-body
                                 tool-name tool-input result-text)))
                (insert (propertize formatted 'read-only t))
                (move-overlay ov (overlay-start ov) (point))))))))))

(defun claude-code--format-tool-body (tool-name tool-input result-text)
  "Format the body for TOOL-NAME with TOOL-INPUT and RESULT-TEXT."
  (let ((result-text (or result-text "")))
    (pcase tool-name
      ((or "Edit" "Write")
       (let ((file-path (alist-get 'file_path tool-input)))
         (concat
          (when file-path
            (concat "  " (claude-code--make-file-link file-path) "\n"))
          (claude-code--fontify-as-diff result-text)
          "\n")))
      ("Bash"
       (let ((cmd (or (alist-get 'command tool-input) "")))
         (concat
          (propertize (format "  $ %s\n" cmd) 'face 'font-lock-function-name-face)
          (claude-code--truncate-output result-text 200)
          "\n")))
      ("Read"
       (let ((file-path (alist-get 'file_path tool-input)))
         (concat
          (when file-path
            (concat "  " (claude-code--make-file-link file-path) "\n"))
          (claude-code--truncate-output result-text 30)
          "\n")))
      (_
       (concat (claude-code--truncate-output result-text 100) "\n")))))

(defun claude-code--toggle-section ()
  "Toggle visibility of the section body at point."
  (interactive)
  (let ((id (or (get-text-property (point) 'claude-code-section-id)
                (save-excursion
                  (beginning-of-line)
                  (get-text-property (point) 'claude-code-section-id)))))
    (if id
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (equal (overlay-get ov 'claude-code-section-id) id)
                     (eq (overlay-get ov 'claude-code-section-type) 'body))
            (let ((currently-hidden (overlay-get ov 'invisible)))
              (overlay-put ov 'invisible (not currently-hidden))
              ;; Update arrow indicator
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (when (and (equal (get-text-property (point) 'claude-code-section-id) id)
                             (eq (get-text-property (point) 'claude-code-section-type) 'header))
                    (let ((inhibit-read-only t))
                      (when (search-forward (if currently-hidden "▸" "▾")
                                            (line-end-position) t)
                        (replace-match (if currently-hidden "▾" "▸")))))
                  (forward-line 1))))))
      ;; Not on a section — fall back to default TAB
      (indent-for-tab-command))))

(defun claude-code--isearch-reveal (ov)
  "Reveal overlay OV during isearch."
  (overlay-put ov 'invisible nil))

;;;; Rendering helpers

(defun claude-code--make-file-link (file-path &optional line-number)
  "Create a clickable link for FILE-PATH, optionally at LINE-NUMBER."
  (let ((display (if line-number
                     (format "%s:%d" file-path line-number)
                   file-path)))
    (propertize display
                'face 'claude-code-file-link-face
                'mouse-face 'highlight
                'help-echo (format "Open %s" file-path)
                'keymap claude-code--link-keymap
                'claude-code-file-path file-path
                'claude-code-line-number line-number)))

(defvar claude-code--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-code--follow-link)
    (define-key map [mouse-1] #'claude-code--follow-link)
    map)
  "Keymap for clickable file links.")

(defun claude-code--follow-link ()
  "Open the file at point in other window, jumping to line if specified."
  (interactive)
  (let ((path (get-text-property (point) 'claude-code-file-path))
        (line (get-text-property (point) 'claude-code-line-number)))
    (when path
      (let ((abs-path (if (file-name-absolute-p path)
                          path
                        (expand-file-name path claude-code--project-root))))
        (find-file-other-window abs-path)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun claude-code--fontify-as-diff (text)
  "Apply diff-mode faces to TEXT if it looks like a diff."
  (if (and (stringp text)
           (string-match-p "^\\(---\\|\\+\\+\\+\\|@@\\|[-+]\\)" text))
      (with-temp-buffer
        (insert text)
        (diff-mode)
        (font-lock-ensure)
        (buffer-string))
    (or text "")))

(defun claude-code--truncate-output (text max-lines)
  "Truncate TEXT to MAX-LINES."
  (if (not (stringp text))
      ""
    (let ((lines (split-string text "\n")))
      (if (<= (length lines) max-lines)
          text
        (concat
         (string-join (seq-take lines max-lines) "\n")
         (propertize (format "\n  ... (%d more lines)" (- (length lines) max-lines))
                     'face 'font-lock-comment-face))))))

(defun claude-code--linkify-paths (text)
  "Replace file paths in TEXT with clickable links."
  (if (not (stringp text))
      ""
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(/[^[:space:]\n]+\\)\\(?::\\([0-9]+\\)\\)?"
              nil t)
        (let* ((path (match-string 1))
               (line-str (match-string 2))
               (line-num (and line-str (string-to-number line-str)))
               (beg (match-beginning 0))
               (end (match-end 0)))
          (when (and path
                     (not (string-prefix-p "//" path))
                     (file-exists-p path))
            (let ((link (claude-code--make-file-link path line-num)))
              (delete-region beg end)
              (goto-char beg)
              (insert link)))))
      (buffer-string))))

;;;; Auto-revert

(defun claude-code--maybe-revert-buffers (tool-name tool-input _result)
  "Revert buffers for files modified by TOOL-NAME with TOOL-INPUT."
  (when (member tool-name '("Edit" "Write"))
    (let* ((file-path (or (alist-get 'file_path tool-input)
                          (alist-get 'path tool-input)))
           (abs-path (and file-path
                          (if (file-name-absolute-p file-path)
                              file-path
                            (expand-file-name file-path claude-code--project-root))))
           (buf (and abs-path (find-buffer-visiting abs-path))))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (if (buffer-modified-p)
              (message "claude-code: %s has unsaved changes, not reverting"
                       (file-name-nondirectory abs-path))
            (revert-buffer t t t)
            (message "claude-code: Reverted %s"
                     (file-name-nondirectory abs-path))))))))

;;;; Session management

(defun claude-code--register-session (session-id buffer)
  "Register SESSION-ID with BUFFER."
  (setq claude-code--sessions
        (cons (cons session-id buffer)
              (cl-remove-if (lambda (entry)
                              (equal (car entry) session-id))
                            claude-code--sessions))))

(defun claude-code--active-sessions ()
  "Return alist of live sessions."
  (cl-remove-if-not
   (lambda (entry)
     (buffer-live-p (cdr entry)))
   claude-code--sessions))

(defun claude-code--session-buffer-name (project-root)
  "Generate a buffer name for PROJECT-ROOT."
  (format "*claude-code: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

;;;; Public commands

;;;###autoload
(defun nj/claude-code-start (&optional project-root)
  "Start a new Claude Code session.
PROJECT-ROOT defaults to projectile-project-root or `default-directory'."
  (interactive)
  (let* ((root (or project-root
                   (and (fboundp 'projectile-project-root)
                        (projectile-project-root))
                   default-directory))
         (buf-name (claude-code--session-buffer-name root))
         (buf (generate-new-buffer buf-name)))
    (claude-code--init-buffer buf root)
    (switch-to-buffer buf)))

;;;###autoload
(defun nj/claude-code-resume ()
  "Resume a previous Claude Code session."
  (interactive)
  (let* ((sessions (claude-code--active-sessions))
         (choices (mapcar (lambda (s)
                           (cons (format "%s [%s]"
                                         (buffer-name (cdr s))
                                         (substring (car s) 0
                                                    (min 8 (length (car s)))))
                                 (car s)))
                          sessions)))
    (if choices
        (let* ((choice (completing-read "Resume session: " choices nil t))
               (session-id (cdr (assoc choice choices)))
               (buf (cdr (assoc session-id claude-code--sessions))))
          (switch-to-buffer buf))
      (let* ((session-id (read-string "Session ID to resume: "))
             (root (or (and (fboundp 'projectile-project-root)
                            (projectile-project-root))
                       default-directory))
             (buf (generate-new-buffer
                   (claude-code--session-buffer-name root))))
        (claude-code--init-buffer buf root)
        (with-current-buffer buf
          (setq claude-code--session-id session-id))
        (switch-to-buffer buf)))))

;;;###autoload
(defun nj/claude-code-switch ()
  "Switch between active Claude Code sessions."
  (interactive)
  (let* ((bufs (cl-remove-if-not
                (lambda (b)
                  (with-current-buffer b
                    (derived-mode-p 'claude-code-mode)))
                (buffer-list)))
         (names (mapcar #'buffer-name bufs)))
    (if names
        (switch-to-buffer (completing-read "Claude session: " names nil t))
      (user-error "No active Claude Code sessions"))))

;;;###autoload
(defun nj/claude-code-kill-session ()
  "Kill the current Claude Code session."
  (interactive)
  (when (derived-mode-p 'claude-code-mode)
    (when (and claude-code--process (process-live-p claude-code--process))
      (kill-process claude-code--process))
    (when claude-code--session-id
      (setq claude-code--sessions
            (cl-remove-if (lambda (entry)
                            (equal (car entry) claude-code--session-id))
                          claude-code--sessions)))
    (kill-buffer (current-buffer))))

(defun claude-code--copy-last-response ()
  "Copy the last assistant response to the kill ring."
  (interactive)
  (save-excursion
    (goto-char claude-code--output-end-marker)
    (let ((end (point))
          (start (point)))
      (when (re-search-backward "^You: " nil t)
        (forward-line 1)
        (setq start (point))
        (let ((text (string-trim (buffer-substring-no-properties start end))))
          (kill-new text)
          (message "Copied %d chars" (length text)))))))

;;;; Tool permission management

(defun claude-code--tool-status (tool)
  "Return the permission status of TOOL as a string."
  (cond
   ((member tool claude-code-always-allowed-tools) "always")
   ((member tool claude-code--session-allowed-tools) "session")
   (t "denied")))

(defun claude-code--read-tool (prompt &optional filter)
  "Prompt for a tool name with PROMPT.
Optional FILTER is a predicate to narrow the candidates."
  (let ((candidates (if filter
                        (cl-remove-if-not filter claude-code--known-tools)
                      claude-code--known-tools)))
    (completing-read prompt candidates nil t)))

(defun claude-code--allow-tool-session (tool)
  "Allow TOOL for the current session only."
  (interactive
   (list (claude-code--read-tool
          "Allow tool (session): "
          (lambda (t) (not (member t (claude-code--effective-allowed-tools)))))))
  (unless (member tool claude-code--session-allowed-tools)
    (push tool claude-code--session-allowed-tools))
  (message "Allowed %s for this session" tool))

(defun claude-code--allow-tool-permanent (tool)
  "Allow TOOL permanently by adding to `claude-code-always-allowed-tools'."
  (interactive
   (list (claude-code--read-tool
          "Allow tool (permanent): "
          (lambda (t) (not (member t claude-code-always-allowed-tools))))))
  (unless (member tool claude-code-always-allowed-tools)
    (customize-save-variable
     'claude-code-always-allowed-tools
     (append claude-code-always-allowed-tools (list tool))))
  ;; Also remove from session list if present (now permanent)
  (setq claude-code--session-allowed-tools
        (delete tool claude-code--session-allowed-tools))
  (message "Allowed %s permanently" tool))

(defun claude-code--revoke-tool-session (tool)
  "Revoke session-level permission for TOOL."
  (interactive
   (list (claude-code--read-tool
          "Revoke tool (session): "
          (lambda (t) (member t claude-code--session-allowed-tools)))))
  (setq claude-code--session-allowed-tools
        (delete tool claude-code--session-allowed-tools))
  (message "Revoked session permission for %s" tool))

(defun claude-code--revoke-tool-permanent (tool)
  "Revoke permanent permission for TOOL."
  (interactive
   (list (claude-code--read-tool
          "Revoke tool (permanent): "
          (lambda (t) (member t claude-code-always-allowed-tools)))))
  (customize-save-variable
   'claude-code-always-allowed-tools
   (delete tool claude-code-always-allowed-tools))
  (message "Revoked permanent permission for %s" tool))

(defun claude-code--tools-preset-readonly ()
  "Set tools to read-only preset: Read, Grep, Glob."
  (interactive)
  (setq claude-code--session-allowed-tools nil)
  (customize-save-variable 'claude-code-always-allowed-tools
                           '("Read" "Grep" "Glob"))
  (message "Tools set to read-only: Read, Grep, Glob"))

(defun claude-code--tools-preset-standard ()
  "Set tools to standard preset: read-only + Edit, Write, Bash."
  (interactive)
  (setq claude-code--session-allowed-tools nil)
  (customize-save-variable 'claude-code-always-allowed-tools
                           '("Read" "Grep" "Glob" "Edit" "Write" "Bash"))
  (message "Tools set to standard: Read, Grep, Glob, Edit, Write, Bash"))

(defun claude-code--tools-preset-all ()
  "Allow all known tools permanently."
  (interactive)
  (setq claude-code--session-allowed-tools nil)
  (customize-save-variable 'claude-code-always-allowed-tools
                           (copy-sequence claude-code--known-tools))
  (message "All tools allowed"))

(defun claude-code--tools-status-description ()
  "Return a formatted string showing current tool permissions."
  (let ((always claude-code-always-allowed-tools)
        (session claude-code--session-allowed-tools)
        (denied (cl-remove-if
                 (lambda (t)
                   (or (member t claude-code-always-allowed-tools)
                       (member t claude-code--session-allowed-tools)))
                 claude-code--known-tools)))
    (concat
     (propertize "Always: " 'face 'success)
     (if always (string-join always ", ") "none")
     "\n"
     (propertize "Session: " 'face 'warning)
     (if session (string-join session ", ") "none")
     "\n"
     (propertize "Denied: " 'face 'error)
     (if denied (string-join denied ", ") "none"))))

;;;###autoload
(transient-define-prefix nj/claude-code-tools-transient ()
  "Manage Claude Code tool permissions."
  [:description claude-code--tools-status-description]
  ["Allow"
   ("a" "Allow for session" claude-code--allow-tool-session)
   ("A" "Allow permanently" claude-code--allow-tool-permanent)]
  ["Revoke"
   ("d" "Revoke session" claude-code--revoke-tool-session)
   ("D" "Revoke permanent" claude-code--revoke-tool-permanent)]
  ["Presets"
   ("1" "Read-only" claude-code--tools-preset-readonly)
   ("2" "Standard (+ Edit, Write, Bash)" claude-code--tools-preset-standard)
   ("3" "All tools" claude-code--tools-preset-all)])

;;;; Transient menu

;;;###autoload
(transient-define-prefix nj/claude-code-transient ()
  "Claude Code session management."
  ["Session"
   ("n" "New session" nj/claude-code-start)
   ("s" "Switch session" nj/claude-code-switch)
   ("r" "Resume session" nj/claude-code-resume)
   ("k" "Kill session" nj/claude-code-kill-session)]
  ["Actions"
   ("w" "Copy last response" claude-code--copy-last-response)
   ("t" "Tool permissions" nj/claude-code-tools-transient)])

;;;; Magit integration

;;;###autoload
(defun nj/claude-code-from-magit ()
  "Launch Claude Code from magit, scoped to the repo."
  (interactive)
  (let ((root (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                  default-directory)))
    (nj/claude-code-start root)))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-dispatch "!"
    '("C" "Claude Code" nj/claude-code-from-magit)))

(provide 'claude-code)
;;; claude-code.el ends here
