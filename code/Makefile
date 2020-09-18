# Adjust the run address to match the .org in the source code
all: test.hex

test.hex: test.asm
	sbasm ./test.asm

clean:
	$(RM) *.bin *.hex *.s19 *.lst

distclean: clean
