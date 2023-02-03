FC = gfortran
FCFLAGS = -pedantic -std=f95 -Wall -Werror -O2 -g
FLFLAGS =
OBJ = driver.o graph.o linkedlist.o
MOD = graph.o linkedlist.o
BIN = driver

$(BIN): $(OBJ)
	$(FC) -o $@ $^ $(FLFLAGS)

driver.o: driver.f90 graph.o
	$(FC) -c $< $(FCFLAGS)

graph.o : graph.f90 linkedlist.o
	$(FC) -c $< $(FCFLAGS)

linkedlist.o: linkedlist.f90
	$(FC) -c $< $(FCFLAGS)

.PHONY: clean
clean:
	$(RM) driver *.o *.mod *~
