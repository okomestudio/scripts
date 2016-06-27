host=`hostname -s`

##############################################################################
# Run setup scripts.
#
# The order of sourcing these scripts matters (e.g., in adding
# directories to PATH).

if [ $host == "ringo" ]; then
    SETUP_FINK=/sw/bin/init.sh
    test -r $SETUP_FINK && . $SETUP_FINK

    SETUP_SCISOFT=/usr/local/scisoft/bin/Setup.bash
    #SETUP_SCISOFT=/Applications/scisoft/all/bin/Setup.bash
    test -r $SETUP_SCISOFT && . $SETUP_SCISOFT
fi

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

##############################################################################
# Misc. Settings

# IRAF
if [ -d /iraf/iraf ]; then
    export iraf=/iraf/iraf/  # where IRAF is installed
    export IRAFARCH=linux
fi

# Uncomment this to use matplotlib backend for PyRAF plotting!
#export PYRAFGRAPHICS=matplotlib

# kcorrect
export KCORRECT_DIR=/home/taro/lib/python2.6/site-packages/kcorrect

export DEEP2_DIR=/astro/DEEP2
