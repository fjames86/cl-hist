====
CL-HIST

A lightweight package for manipulating histograms. Supports linear and 2d histograms. 

All 2D functions can be accessed with the -2d suffix
* Create using (make-hist start end num) to create an empty histogram from start to end with num bins
* Update using (update-hist hist val &optional amount)
  Amount is typically 1 but can be any (positive) float to allow for e.g. variable stepsize sampling
* Print out to a stream using (output-hist hist stream)
  Will print out 3 columns, <x coord> <raw hist value> <ratio value>
* Histograms can be cleared and reused with (reset-hist hist)
* Histograms can be manipulated using (maphist function hist)

Frank James


  