LN	= ln -f
SOURCE	= xmonad.hs
TARGET	= $(HOME)/.xmonad/xmonad.hs

.PHONY: all

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(LN) $(SOURCE) $(TARGET)
