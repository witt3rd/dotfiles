sudo apt update

sudo apt upgrade -y

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt install emacs27

sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test

sudo apt install -y gcc-11 g++-11

wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add - # Fingerprint: 6084 F3CF 814B 57C1 CF12 EFD5 15CF 4D18 AF4F 7421

sudo apt-add-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal main"
sudo apt-get install clang-format-14 clang-tidy-14 clang-tools-14 clang-14 clangd-14 libc++-14-dev libc++1-14 libc++abi-14-dev libc++abi1-14 libclang-14-dev libclang1-14 liblldb-14-dev libllvm-14-ocaml-dev libomp-14-dev libomp5-14 lld-14 lldb-14 llvm-14-dev llvm-runtime llvm-14 python-clang

#sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-12 100
