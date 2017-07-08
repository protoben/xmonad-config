LN	= ln -f
SOURCE	= xmonad.hs
TARGET	= $(HOME)/.xmonad/xmonad.hs

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(LN) $(SOURCE) $(TARGET)
