export MEM_SZ="8G"
export CPU_N=4

docker build -t qemu-misc -f prepare-qemu/dockerfile-qemu prepare-qemu/
docker run --rm -it --name $(whoami)-ubuntu-aarch64 \
    -p 22225:22225 \
    qemu-misc \
    /bin/bash -c "\
qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a57 \
    -m ${MEM_SZ} \
    -smp cpus=${CPU_N} \
    -accel tcg,thread=multi \
    -nographic \
    -drive if=pflash,format=raw,file=efi.img,readonly=on \
    -drive if=pflash,format=raw,file=varstore.img \
    -drive if=none,file=ubuntu-25.04-minimal-cloudimg-arm64.img,format=qcow2,id=hd0 \
    -device virtio-blk-device,drive=hd0 \
    -drive file=seed.iso,media=cdrom,if=none,id=cd0 \
    -device virtio-scsi-device \
    -device scsi-cd,drive=cd0 \
    -device virtio-net-device,netdev=eth0 \
    -netdev user,id=eth0,hostfwd=tcp::22225-:22 \
    -device virtio-rng-device\
"