export MEM_SZ="8G"
export CPU_N=6
docker build -t qemu-misc -f prepare-qemu/dockerfile-qemu prepare-qemu/
docker run -it --name $(whoami)-fedora-rvcpu \
    -p 22224:22224 \
    qemu-misc \
    /bin/bash -c "\
qemu-system-riscv64 \
    -machine virt \
    -m ${MEM_SZ} \
    -smp ${CPU_N} \
    -accel tcg,thread=multi \
    -nographic \
    -kernel u-boot.elf \
    -device virtio-net-device,netdev=eth0 -netdev user,id=eth0,hostfwd=tcp::22224-:22 \
    -device virtio-rng-pci \
    -drive file=Fedora-Cloud-Base-UEFI-UKI-42.riscv64.qcow2,if=virtio \
    -cdrom seed.iso\
"