#!/bin/bash
# https://www.qemu.org/docs/master/system/riscv/virt.html
# https://documentation.ubuntu.com/server/how-to/virtualisation/qemu/index.html
# https://wiki.ubuntu.com/RISC-V/QEMU
# assuming . is the root of repo
set -x
WORKDIR=./prepare-qemu

export QEMU_ENV_PATH=./qemu-env
export RISCV_PATH=${QEMU_ENV_PATH}/riscv64
export FEDORA_URL="https://dl.fedoraproject.org/pub/alt/risc-v/release/42/Cloud/riscv64/images/Fedora-Cloud-Base-UEFI-UKI-42.20250414-8635a3a5bfcd.riscv64.qcow2"
export FEDORA_NAME="Fedora-Cloud-Base-UEFI-UKI-42.riscv64.qcow2"
export FEDORA_PATH="${RISCV_PATH}/${FEDORA_NAME}"
export RV64_UBOOT_ELF="${RISCV_PATH}/u-boot.elf"
export RV64_UBOOT_BIN="${RISCV_PATH}/u-boot.bin"

cloud-localds ${QEMU_ENV_PATH}/seed.iso ${WORKDIR}/user-data.yaml ${WORKDIR}/meta-data.yaml
mkdir -p ${RISCV_PATH}
if [ ! -f "$RV64_UBOOT_ELF" ] || [ ! -f "$RV64_UBOOT_BIN" ] ; then
    sudo \
        docker build -t uboot -f ${WORKDIR}/dockerfile-just-uboot ${WORKDIR}/ && \
        docker create --name extract uboot && \
        docker cp extract:/usr/lib/u-boot/qemu-riscv64_smode/uboot.elf "${RV64_UBOOT_ELF}" && \
        docker cp extract:/usr/lib/u-boot/qemu-riscv64_smode/u-boot.bin "${RV64_UBOOT_BIN}"
    sudo docker rm extract
    sudo chown -R $(whoami):$(whoami) ${QEMU_ENV_PATH}
fi

if [ ! -f "${FEDORA_PATH}" ]; then
    wget -O ${FEDORA_PATH} ${FEDORA_URL}
fi

qemu-img resize ${FEDORA_PATH} 20G

qemu-system-riscv64 \
    -machine virt \
    -m 4G \
    -smp cpus=4 \
    -accel tcg,thread=multi \
    -nographic \
    -kernel "${RV64_UBOOT_ELF}" \
    -device virtio-net-device,netdev=eth1 -netdev user,id=eth1,hostfwd=tcp::22222-:22 \
    -device virtio-rng-pci \
    -drive file=${FEDORA_PATH},if=virtio \
    -cdrom ${QEMU_ENV_PATH}/seed.iso

exit 0
# NOTE: add ssh public key into the user-data.yaml
# NOTE: this is how you can do rsync
rsync -avz \
    -e "ssh -p 22222" \
    --exclude 'qemu-env' \
    --exclude '.git' \
    $PWD root@localhost:~