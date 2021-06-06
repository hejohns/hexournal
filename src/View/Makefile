.PHONY: clean

CC := gcc
CFLAGS := -std=c11 -ggdb

crop: crop.c
	$(CC) $(CFLAGS) `pkg-config --cflags cairo` -o $@ $^ $(LDFLAGS) `pkg-config --libs cairo`

clean:
	rm crop
