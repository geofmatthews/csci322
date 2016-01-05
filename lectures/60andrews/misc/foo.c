double a[n,n], b[n,n], c[n,n];
for [i = 0 to n-1] {
  for [j = 0 to n-1] {
# compute inner product of a[i,*] and b[*,j]
      c[i,j] = 0.0;
      for [k = 0 to n-1]
	c[i,j] = c[i,j] + a[i,k]*b[k,j];
    }
}


co [i = 0 to n-1] { # compute rows in parallel
  for [j = 0 to n-1] {
      c[i,j] = 0.0;
      for [k = 0 to n-1]
	c[i,j] = c[i,j] + a[i,k]*b[k,j];
    }
}


co [i = 0 to n-1] { # compute rows in parallel
  for [j = 0 to n-1] {
      c[i,j] = 0.0;
      for [k = 0 to n-1]
	c[i,j] = c[i,j] + a[i,k]*b[k,j];
    }
}

co [j = 0 to n-1] { # compute columns in parallel
  for [i = 0 to n-1] {
      c[i,j] = 0.0;
      for [k = 0 to n-1]
	c[i,j] = c[i,j] + a[i,k]*b[k,j];
    }
}

co [i = 0 to n-1, j = 0 to n-1] { # all rows and
  c[i,j] = 0.0; # all columns
  for [k = 0 to n-1]
    c[i,j] = c[i,j] + a[i,k]*b[k,j];
}

process row[i = 0 to n-1] { # rows in parallel
  for [j = 0 to n-1] {
      c[i,j] = 0.0;
      for [k = 0 to n-1]
	c[i,j] = c[ij] + a[i,k]*b[k,j];
    }
}

process worker[w = 1 to P] { // strips in parallel
  int first = (w-1) * n/P; // first row of strip
  int last = first + n/P - 1; // last row of strip
  for [i = first to last] {
      for [j = 0 to n-1] {
	  c[i,j] = 0.0;
	  for [k = 0 to n-1]
	    c[i,j] = c[i,j] + a[i,k]*b[k,j];
	}
    }
}

double fleft = f(a), fright, area = 0.0;
double width = (b-a) / INTERVALS;
for [x = (a + width) to b by width] {
  fright = f(x);
  area = area + (fleft + fright) * width / 2;
  fleft = fright;
}
