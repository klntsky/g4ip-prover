all: build

build:
	happy src/Parser.y
	cabal build


clean:
	rm -rf G4ip/*.o G4ip/*.hi g4ip G4ip/Tests
	rm -rf *.o *.hi

pdf: build
	./g4ip
	./build.sh
