# For WSL users
# $sudo nohup dockerd &
# use docker as usual
# # how to use
# docker build -t fedora-chezscheme .
# # --ipc=host is to allow the use of /dev/shm/
# # to run
# sudo docker run --rm --ipc=host -it -v $PWD:$PWD -w $PWD fedora-chezscheme bash

FROM fedora:latest

RUN dnf install -y \
    chez-scheme.x86_64 \
    git gcc glibc-devel.i686 libgcc.i686 \
    diff ncurses

CMD ["/bin/bash"]
