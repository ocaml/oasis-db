all: rating-bgd.png rating-fgd-green.png

rating-bgd.png: rating-fgd.png
	convert $^ -colorspace Gray $@

rating-fgd-green.png: rating-bgd.png
	convert $^ -colorize 0,0,50 -colorspace RGB $@
