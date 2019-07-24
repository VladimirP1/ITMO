import std.range;
import std.stdio;
import std.conv;
import std.array;
import std.algorithm.mutation;
import std.algorithm.iteration;

int[] sieve;

void generate() {
    sieve.length = 20000005;
    fill(sieve, 1);
    
    for (int i = 2; i*i <= sieve.length; ++i) if (sieve[i] == 1) {
        int z = i*i;
        while(z < sieve.length) {
            sieve[z] = i;
            z += i;
        }
    }
}

void main() {
    int n;

    generate();

    auto lines = array(map!"to!int(strip(a))"(stdin.byLine()));
    
    n = lines[0];
    
    foreach (x ; dropOne(lines)) {
        if (sieve[x] == 1) {
            printf("YES\n");
        } else {
            printf("NO\n");
        }
    }
}
