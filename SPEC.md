# ClaudeCrons - Product Spec

## Overview

ClaudeCrons is a mobile-first PWA that lets users create scheduled Claude Code agent runs ("crons") that fire at a given cadence, execute standalone prompts via the Claude Code CLI, and deliver push notification summaries of the results.

---

## Tech Stack

| Layer        | Technology                              |
| ------------ | --------------------------------------- |
| Frontend     | React (mobile-first PWA, service worker for push) |
| Backend API  | FastAPI (Python)                        |
| Database     | PostgreSQL                              |
| Task Queue   | Celery + Redis (or similar)             |
| Execution    | Claude Code CLI (`claude` binary)       |
| Deployment   | Render.com (web service + background worker + managed Postgres) |

---

## Architecture

### High-Level Flow

```
User (mobile PWA)
    │
    ▼
FastAPI (Render web service)
    │
    ├── CRUD crons, auth, history
    ├── Push subscription management
    │
    ▼
Task Scheduler (Celery Beat / APScheduler)
    │
    ▼
Worker Pool (Render background worker, fixed concurrency)
    │
    ├── Fetch URL context (if configured)
    ├── Shell out to `claude --dangerously-skip-permissions --print` with prompt
    ├── Capture full output + token usage
    ├── Generate LLM summary (cheap model call)
    ├── Store results in Postgres
    └── Send push notification with summary
```

### Execution Model

- **CLI invocation**: The worker runs `claude` CLI in `--print` mode with `--dangerously-skip-permissions` flag
- **No Docker sandboxing**: Runs directly on the Render worker with a non-root user and restricted filesystem access
- **Concurrency**: Fixed pool (e.g., 3 concurrent runs), additional runs queued
- **Timeout**: 10-minute hard kill per run
- **Failure handling**: Notify the user of the failure, skip to next scheduled run (no retries)

### URL Context Fetching

- Crons can optionally include one or more URLs as context sources
- URLs are fetched **fresh before each run** so context is always current
- Fetched content is prepended to the user's prompt before CLI execution

---

## User Management

### Auth

- **Method**: Username + password (standard bcrypt-hashed credentials)
- **Signup gating**: YAML-based email whitelist (`allowed_emails.yaml`). Only whitelisted emails can register
- **Sessions**: JWT tokens (short-lived access + refresh token)

### Multi-User

- Full user accounts with isolated data (each user only sees their own crons and runs)
- Shared infrastructure (single DB, single worker pool)

---

## Data Model

### Users

| Column         | Type      | Notes                     |
| -------------- | --------- | ------------------------- |
| id             | UUID (PK) |                           |
| email          | VARCHAR   | Unique, must be in whitelist |
| username       | VARCHAR   | Unique                    |
| password_hash  | VARCHAR   | bcrypt                    |
| anthropic_api_key | VARCHAR (encrypted) | Per-user, used for CLI runs |
| created_at     | TIMESTAMP |                           |

### Crons

| Column         | Type      | Notes                     |
| -------------- | --------- | ------------------------- |
| id             | UUID (PK) |                           |
| user_id        | UUID (FK) |                           |
| name           | VARCHAR   | User-friendly label       |
| prompt         | TEXT      | The prompt to execute     |
| schedule_type  | ENUM      | `hourly`, `daily`, `weekly`, `custom` |
| schedule_config| JSONB     | Day of week, time, interval details |
| context_urls   | TEXT[]    | Optional URLs to fetch before each run |
| is_active      | BOOLEAN   | Default true              |
| created_at     | TIMESTAMP |                           |
| updated_at     | TIMESTAMP |                           |

### Cron Runs

| Column         | Type      | Notes                     |
| -------------- | --------- | ------------------------- |
| id             | UUID (PK) |                           |
| cron_id        | UUID (FK) |                           |
| user_id        | UUID (FK) | Denormalized for query perf |
| status         | ENUM      | `running`, `success`, `failed`, `timeout` |
| full_output    | TEXT      | Complete CLI output        |
| summary        | TEXT      | LLM-generated 2-3 sentence summary |
| token_usage    | JSONB     | `{input_tokens, output_tokens, cost_usd}` |
| started_at     | TIMESTAMP |                           |
| completed_at   | TIMESTAMP |                           |
| error_message  | TEXT      | Populated on failure       |
| is_manual      | BOOLEAN   | True if triggered manually |

### Push Subscriptions

| Column         | Type      | Notes                     |
| -------------- | --------- | ------------------------- |
| id             | UUID (PK) |                           |
| user_id        | UUID (FK) |                           |
| subscription   | JSONB     | Web Push subscription object |
| created_at     | TIMESTAMP |                           |

---

## API Endpoints

### Auth

- `POST /api/auth/register` - Register (validates email against whitelist)
- `POST /api/auth/login` - Login, returns JWT pair
- `POST /api/auth/refresh` - Refresh access token

### Crons

- `GET /api/crons` - List user's crons
- `POST /api/crons` - Create a cron (enforces hard limit per user)
- `GET /api/crons/{id}` - Get cron details
- `PUT /api/crons/{id}` - Update cron (name, prompt, schedule, URLs)
- `DELETE /api/crons/{id}` - Delete cron
- `POST /api/crons/{id}/trigger` - Manual trigger (immediate test run)

### Runs

- `GET /api/crons/{id}/runs` - List runs for a cron (paginated)
- `GET /api/runs` - Global activity feed across all user's crons (paginated, newest first)
- `GET /api/runs/{id}` - Get full run detail (including full output)

### User Settings

- `GET /api/me` - Get current user profile
- `PUT /api/me` - Update profile (username, API key)
- `GET /api/me/usage` - Aggregate token usage / cost summary

### Push

- `POST /api/push/subscribe` - Register push subscription
- `DELETE /api/push/subscribe` - Unregister push subscription

---

## Frontend (PWA)

### Navigation

Bottom tab bar with 4 tabs:

| Tab       | Screen             | Description                          |
| --------- | ------------------ | ------------------------------------ |
| Feed      | Activity Feed      | Card-based feed of recent runs across all crons |
| Crons     | My Crons           | List of all user's crons with status |
| + (Create)| Create Cron        | Form to create a new cron            |
| Settings  | Settings           | Profile, API key, notification prefs |

### Screens

#### Activity Feed (Home)

- Card-based feed of all recent cron runs, newest first
- Each card shows: cron name, status badge (success/fail/timeout), relative time, LLM summary
- Tap a card to view full output (rendered markdown)
- Pull-to-refresh; data loads fresh on each page visit (poll on load)
- Empty state with prompt to create first cron

#### My Crons

- List of cron cards showing: name, schedule description (e.g., "Every day at 9am"), active/paused status, last run time
- Tap a cron to view its detail page (edit form + run history for that cron)
- Swipe-to-delete or delete button

#### Create Cron

- **Name**: Text input
- **Prompt**: Multi-line textarea
- **Schedule**: Preset selector (hourly, daily, weekly) with time/day picker for customization
- **Context URLs** (optional): Add one or more URLs
- Validation: enforce max cron limit, require name + prompt + schedule

#### Cron Detail

- Editable fields (name, prompt, schedule, URLs)
- "Run Now" button for manual trigger
- Run history for this specific cron (same card format as feed)

#### Run Detail

- Full output rendered as markdown
- Metadata: duration, token usage, cost, timestamp
- Status indicator

#### Settings

- Edit username
- Manage Anthropic API key (masked display, update form)
- Usage dashboard: total runs, total cost, cost per cron breakdown
- Push notification toggle
- Logout

### PWA Features

- Service worker for push notifications (Web Push API)
- Add-to-homescreen manifest
- Offline shell (app chrome loads offline, data requires connectivity)

---

## Scheduling

### Preset Intervals

| Preset  | Default Behavior               |
| ------- | ------------------------------ |
| Hourly  | Every hour on the hour         |
| Daily   | Every day at user-selected time |
| Weekly  | Every week on user-selected day + time |
| Custom  | User picks specific days + time (e.g., Mon/Wed/Fri at 10am) |

- All times stored in UTC, displayed in user's local timezone
- Schedule config stored as JSONB for flexibility

---

## Push Notifications

- **Protocol**: Web Push API (VAPID keys)
- **Trigger**: After each cron run completes (success or failure)
- **Content**:
  - **Title**: Cron name + status emoji (checkmark or X)
  - **Body**: LLM-generated summary (2-3 sentences)
  - **Action**: Tap opens the run detail page in the PWA

---

## Cost Tracking

- Per-user Anthropic API keys: each user's runs bill to their own key
- Token usage captured from CLI output after each run
- Cost calculated using Anthropic's published pricing
- Displayed in:
  - Each run card (token count + estimated cost)
  - Settings > Usage dashboard (aggregate view)
- **No enforcement** - display only, users manage their own API spend

---

## Limits & Constraints

| Constraint               | Value     |
| ------------------------ | --------- |
| Max crons per user       | 25        |
| Max execution time       | 10 min    |
| Worker concurrency       | 3 parallel runs |
| Max prompt length        | 10,000 chars |
| Max context URLs per cron| 5         |
| Run history retention    | Forever   |
| Min schedule interval    | 1 hour    |

---

## Deployment (Render.com)

### Services

1. **Web Service**: FastAPI app serving API + static PWA build
2. **Background Worker**: Celery worker for cron execution + scheduling
3. **Managed PostgreSQL**: Render Postgres instance
4. **Redis**: Render Redis instance (Celery broker + result backend)

### Environment Variables

- `DATABASE_URL` - Postgres connection string
- `REDIS_URL` - Redis connection string
- `VAPID_PRIVATE_KEY` / `VAPID_PUBLIC_KEY` - Web Push keys
- `JWT_SECRET` - Token signing key
- `ALLOWED_EMAILS_PATH` - Path to email whitelist YAML
- `MAX_CRONS_PER_USER` - Configurable limit (default 25)
- `WORKER_CONCURRENCY` - Parallel run limit (default 3)
- `RUN_TIMEOUT_SECONDS` - Max execution time (default 600)

### Security (No Docker)

Since runs execute directly on the Render worker (no Docker):

- Worker runs as a non-root user with restricted filesystem permissions
- Each CLI invocation runs in a temporary directory that's cleaned up after completion
- `--dangerously-skip-permissions` is safe here because the prompt is user-authored (users run their own prompts with their own API keys)
- No network restrictions (CLI needs outbound HTTPS for API calls)
- API key stored encrypted at rest in Postgres

---

## Future Considerations (Out of Scope for V1)

- WebSocket real-time feed updates
- Pause/resume crons
- Cron duplication/cloning
- Multi-step workflows
- Repo-targeted runs
- Slack/email notification channels
- Per-cron retention policies
- Docker sandboxing for untrusted prompts
