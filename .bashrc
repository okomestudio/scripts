
# Add autocomplete to install/script
complete -W \
         "$(find bin/ -type f -printf '%f ')" \
         ./install/script

complete -W \
         "$(find bin/ -type f -printf '%f ')" \
         install/script
