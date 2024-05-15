# Makefile

# Variables
GHC = ghc
SRC = Main.hs PreprocessData.hs CompareWeeks.hs NormalizeKey.hs
EXEC = main

# Default target
all: $(EXEC)

# Rule to build the executable
$(EXEC): $(SRC)
	$(GHC) --make Main.hs -o $(EXEC)

# Rule to clean up generated files
clean:
	rm -f *.hi *.o $(EXEC)

# Phony targets
.PHONY: all clean
