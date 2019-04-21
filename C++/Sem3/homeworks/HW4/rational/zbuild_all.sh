set -e
FLAGS="-DBUILD_SHARED_LIBS=ON"
mkdir -p build_librational build_libpoly build_rational_poly
cd build_librational
cmake $FLAGS -D CMAKE_INSTALL_PREFIX=$PWD/../install -D CMAKE_PREFIX_PATH=$PWD/../install/share  ../lib_rational/ 
make
cmake -D COMPONENT=developer -P cmake_install.cmake
cmake -D COMPONENT=user -P cmake_install.cmake
cd -
cd build_libpoly
cmake $FLAGS -D CMAKE_INSTALL_PREFIX=$PWD/../install -D CMAKE_PREFIX_PATH=$PWD/../install/share  ../lib_poly/ 
make
cmake -D COMPONENT=developer -P cmake_install.cmake
cmake -D COMPONENT=user -P cmake_install.cmake
cd -
cd build_rational_poly
cmake $FLAGS -D CMAKE_INSTALL_PREFIX=$PWD/../install -D CMAKE_PREFIX_PATH=$PWD/../install/share  ../rational_poly/ 
make
cmake -P cmake_install.cmake 
cd -
 
