
let spf fmt = 
  Printf.sprintf fmt 

let min_calendar c1 c2 = 
  if CalendarLib.Calendar.compare c1 c2 < 0 then
    c1
  else
    c2
