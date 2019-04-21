#include <iostream>
#include <array>

int n = 0;
std::array<std::array<int, 100>, 100> matrix;

void read() {
    std::cin >> n;
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            std::cin >> matrix[i][j];
        }
    }
}

void floyd() {
    for(int k = 0; k < n; k++) {
        for(int i = 0; i < n; i++) {
            for(int j = 0; j < n; j++) {
                matrix[i][j] = std::min(matrix[i][j], matrix[i][k] + matrix[k][j]);
            }
        }
    }
}

void write() {
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            std::cout << matrix[i][j] << " ";
        }
        std::cout << std::endl;
    }
}


int main()
{
    read();
    floyd();
    write();
    return 0;
}
