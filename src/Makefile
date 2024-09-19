
all: unirtrouver_labyrinthe

unirtrouver.o : unirtrouver.c unirtrouver.h
	gcc -c unirtrouver.c

unirtrouver_labyrinthe: unirtrouver_labyrinthe.c unirtrouver.o
	gcc -g -o $@ unirtrouver.o $@.c

clean:
	rm -f unirtrouver_labyrinthe
	rm -f *.o
