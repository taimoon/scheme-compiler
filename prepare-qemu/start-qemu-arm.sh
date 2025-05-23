# see dockerfile-qemu-arm
### aarch64 ubuntu
### https://cloud-images.ubuntu.com/
set -x
WORKDIR=./prepare-qemu

export QEMU_ENV_PATH=./qemu-env
export ARM_PATH=${QEMU_ENV_PATH}/arm64
export UBUNTU_URL="https://cloud-images.ubuntu.com/minimal/releases/plucky/release/ubuntu-25.04-minimal-cloudimg-arm64.img"
export UBUNTU_NAME="ubuntu-25.04-minimal-cloudimg-arm64.qcow2"
export UBUNTU_PATH="${ARM_PATH}/${UBUNTU_NAME}"

mkdir -p ${QEMU_ENV_PATH}
mkdir -p ${ARM_PATH}

if [ ! -f "${UBUNTU_PATH}" ]; then
    wget -O ${UBUNTU_PATH} ${UBUNTU_URL}
fi

if [ ! -f ${ARM_PATH}/efi.img ] || [ ! -f ${ARM_PATH}/varstore.img ] ; then
    truncate -s 64m ${ARM_PATH}/varstore.img && \
    truncate -s 64m ${ARM_PATH}/efi.img && \
    dd if=/usr/share/edk2/aarch64/QEMU_EFI.fd of=${ARM_PATH}/efi.img conv=notrunc &&\
    dd if=/usr/share/edk2/aarch64/QEMU_VARS.fd of=${ARM_PATH}/varstore.img conv=notrunc
fi

qemu-img resize ${UBUNTU_PATH} 20G

cloud-localds ${QEMU_ENV_PATH}/seed.iso ${WORKDIR}/user-data.yaml ${WORKDIR}/meta-data.yaml

qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a57 \
    -m 8G \
    -smp cpus=4 \
    -accel tcg,thread=multi \
    -nographic \
    -drive if=pflash,format=raw,file=${ARM_PATH}/efi.img,readonly=on \
    -drive if=pflash,format=raw,file=${ARM_PATH}/varstore.img \
    -drive if=none,file=${UBUNTU_PATH},format=qcow2,id=hd0 \
    -device virtio-blk-device,drive=hd0 \
    -drive file=${QEMU_ENV_PATH}/seed.iso,media=cdrom,if=none,id=cd0 \
    -device virtio-scsi-device \
    -device scsi-cd,drive=cd0 \
    -device virtio-net-device,netdev=eth0 \
    -netdev user,id=eth0,hostfwd=tcp::22223-:22 \
    -device virtio-rng-device

exit 0
