
OVM_CFLAGS	= -g
#OVM_CFLAGS	= -O3

INDENT_FLAGS = -nbad -bap -nbc -bbo -bl -bli2 -bls -ncdb -nce -cp1 -cs -di2 -ndj -nfc1 -nfca -hnl -i2 -ip5 -lp -pcs -psl -nsc -nsob 

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

indent:
	indent $(INDENT_FLAGS) ovm.c
	indent $(INDENT_FLAGS) ovm.h
	indent $(INDENT_FLAGS) test.c
