# Compiler settings
FC = $(shell nf-config --fc)
FCLAGS=-O2

# Directories
SRCDIR = ../../src
TESTDIR = .

# Files
TARGET = test_target_mesh
OBJS = $(SRCDIR)/target_mesh.o

# Targets
all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FCFLAGS) -o $(TARGET) -I$(SRCDIR) $(OBJS) $(TESTDIR)/test_target_mesh.F90

clean:
	rm -f $(TARGET)
