
OVM_CFLAGS	= -g
#OVM_CFLAGS	= -O3

libovm.so: ovm.c
	gcc $(OVM_CFLAGS) -fPIC -c ovm.c
	gcc -shared -o libovm.so ovm.o

test:	test.c libovm.so
	gcc -g test.c -L. -lovm -o test

run_test:
	make test
	export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.; ./test

clean:
	rm -f *.o *.so test