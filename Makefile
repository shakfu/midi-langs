
.PHONY: all clean test libremidi

# Directories
LIBREMIDI_DIR = thirdparty/libremidi
LIBREMIDI_BUILD = $(LIBREMIDI_DIR)/build
LIBREMIDI_LIB = $(LIBREMIDI_BUILD)/liblibremidi.a
LIBREMIDI_INCLUDE = $(LIBREMIDI_DIR)/include

# Compiler flags
CFLAGS = -Wall -I$(LIBREMIDI_INCLUDE)
LDFLAGS = -L$(LIBREMIDI_BUILD) -llibremidi -framework CoreMIDI -framework CoreFoundation -framework CoreAudio -lc++ -lreadline

all: forth midi_forth

forth:
	@gcc -o forth forth.c

midi_forth: $(LIBREMIDI_LIB) midi_forth.c
	@gcc $(CFLAGS) -o midi_forth midi_forth.c $(LDFLAGS)

$(LIBREMIDI_LIB): libremidi

libremidi:
	@mkdir -p $(LIBREMIDI_BUILD)
	@cd $(LIBREMIDI_BUILD) && cmake .. -DCMAKE_BUILD_TYPE=Release -DLIBREMIDI_EXAMPLES=OFF -DLIBREMIDI_TESTS=OFF > /dev/null 2>&1
	@$(MAKE) -C $(LIBREMIDI_BUILD) -j4 > /dev/null 2>&1
	@echo "libremidi built"

clean:
	@rm -f forth midi_forth
	@rm -rf $(LIBREMIDI_BUILD)

test: midi_forth
	@./tests/test_midi_forth.sh
