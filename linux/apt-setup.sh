sudo apt update

sudo apt upgrade -y

sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test

sudo apt install -y gcc-11 g++-11

wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add - # Fingerprint: 6084 F3CF 814B 57C1 CF12 EFD5 15CF 4D18 AF4F 7421


