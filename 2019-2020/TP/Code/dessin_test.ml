#load "graphics.cma";;
open Graphics;;
open_graph " 800x600";;

set_line_width 3;;
set_color blue;;
moveto 50 50;
lineto 50 150;;
lineto 80 120;;
lineto 110 150;;
lineto 110 50;;
set_color green;;
moveto 130 50;
lineto 130 150;;
lineto 190 150;;
lineto 190 110;;
lineto 130 110;;
wait_next_event [Button_down];;
close_graph();;
