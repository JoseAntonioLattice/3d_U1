FC = gfortran
SRCMODS =~/Documents/Fortran/modules/src
SRC=./src
BIN=bin

TARGET = 3dU1.exe


SOURCE= $(wildcard $(SRCMODS)/precision_mod.f95 $(SRCMODS)/maths_mod.f95 $(SRCMODS)/check_files_directories_mod.f95 $(SRCMODS)/number2string_mod.f95 $(SRCMODS)/periodic_boundary_conditions_mod.f95 $(SRC)/parameters_mod.f95 $(SRC)/observables_mod.f95  $(SRC)/arrays_mod.f95 $(SRC)/cooling_mod.f95 $(SRC)/create_files.f95 $(SRC)/plaquette.f95 $(SRC)/local_update_algorithms.f95 $(SRC)/memory_mod.f95 $(SRC)/read_input.f95 $(SRC)/dynamics_mod.f95 $(SRC)/main.f95)
OBJECT = $(patsubst %,$(BIN)/%, $(notdir $(SOURCE:.f95=.o)))

FFLAGS = -J$(BIN) -I$(BIN)


$(BIN)/$(TARGET): $(OBJECT)
	$(FC) -o $@ $^


$(BIN)/%.o: $(SRC)/%.f95
	$(FC) $(FFLAGS) -c $< -o $@


$(BIN)/%.o: $(SRCMODS)/%.f95
	$(FC) $(FFLAGS) -c $< -o $@

.PHONY : help run clean

run :
	$(BIN)/$(TARGET)

help :
	@echo "src: $(SOURCE)"
	@echo "bin: $(OBJECT)"

clean :
	rm -f $(OBJECT) $(BIN)/$(TARGET)
