FROM fedora:latest

RUN dnf install -y \
    chez-scheme.x86_64 \
    git gcc glibc-devel.i686 libgcc.i686 \
    diff ncurses

CMD ["/bin/bash"]
