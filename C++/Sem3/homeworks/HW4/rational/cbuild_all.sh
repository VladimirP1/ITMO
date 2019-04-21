set -e
mkdir build 
cd build
FLAGS="-DBUILD_SHARED_LIBS=ON"
cmake -D CMAKE_INSTALL_PREFIX=$PWD/../install $FLAGS ..
make -j4
cmake -D COMPONENT=user -P cmake_install.cmake

