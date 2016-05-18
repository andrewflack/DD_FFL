#include <R.h>
#include <Rmath.h>

void elo_c(int *np, int *nr, int *white, int *black, double *score, 
           double *crats, double *gamma, double *dscore);

{
  double *escore;
  double *ascore;
  int k;
  
  escore = (double *)R_alloc(*np, sizeof(double));
  ascore = (double *)R_alloc(*np, sizeof(double));
  
  for(k=0;k<*np;k++) {  
    escore[k] = 0;
    ascore[k] = 0;
  }  
  
  for(k=0;k<*nr;k++) {
    ascore[white[k]] = ascore[white[k]] + score[k];
    escore[white[k]] = escore[white[k]] + 
      1/(1 + R_pow(10,(crats[black[k]] - crats[white[k]] - gamma[k])/400));
    ascore[black[k]] = ascore[black[k]] + 1 - score[k];
    escore[black[k]] = escore[black[k]] + 
      1/(1 + R_pow(10,(crats[white[k]] - crats[black[k]] + gamma[k])/400)); 
  }
  for(k=0;k<*np;k++) dscore[k] = ascore[k] - escore[k];
}