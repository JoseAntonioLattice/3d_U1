binwidth=0.1
bin(x,width)=width*floor(x/width) + width/2.0

set boxwidth binwidth

plot 'data.dat' using (bin($1,binwidth)):(1) smooth freq with boxes
