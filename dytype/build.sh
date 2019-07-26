while true; do
    rlwrap guile dytype.scm
    if [[ $? -eq 0 ]]
    then continue
    else
        echo -n "Press ENTER to continue"
        read
    fi
done
