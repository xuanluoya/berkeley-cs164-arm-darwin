ASM     = program.s
OBJ     = program.o
BIN     = program
RUNTIME = runtime.c

AS      = as
CC      = clang

all: $(BIN)

$(BIN): $(OBJ) $(RUNTIME)
	$(CC) $(OBJ) $(RUNTIME) -o $(BIN)

$(OBJ): $(ASM)
	$(AS) $(ASM) -o $(OBJ)

exec_compile:
	dune exec berkeley-cs164-arm-darwin

run: exec_compile $(BIN)
	./$(BIN)

clean:
	rm -f $(OBJ) $(BIN)
