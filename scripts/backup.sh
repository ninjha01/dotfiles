#!/bin/bash
# -*-sh-*-
cp -r ~/.emacs.d ~/Google\ Drive/Archives/Shell\ Scripts/custom/;
cp ~/.bash_profile ~/Google\ Drive/Archives/Shell\ Scripts/custom/;
cp ~/.emacs ~/Google\ Drive/Archives/Shell\ Scripts/custom/;
echo "Backed up Emacs/Bash profiles!";
echo "Zipping...";
zip -eqr9 ~/drive.zip ~/Google\ Drive/;
echo "Zipping Complete! Moving file to CHARON";
cp ~/drive.zip ///Volumes/CHARON/;
echo "Successfully moved! Unmounting disk";
#diskutil unmount ///Volumes/CHARON/;
exit;

