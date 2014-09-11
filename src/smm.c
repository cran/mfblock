#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <string.h>
#include <R_ext/Rdynload.h>

double corr__1(double * x, double * y,int length){

double s=0;

for(int i=0;i<length;i++)s+=x[i]*y[i];

return s*12/length-3;

}

void quant__1(double * x, double * y,double *q,const int length_1,const int length_2,double * ans){

int qq;

for(int i=0;i<length_2;i++){
qq=0;
if(q[i]>0.5){

for(int j=0;j<length_1;j++){
if(x[j]>q[i] && y[j]>q[i]){qq++;};
}
ans[i]=qq/(length_1*(1-q[i]));

}
else{
for(int j=0;j<length_1;j++){
if(x[j]<=q[i] && y[j]<=q[i]){qq++;};
}
ans[i]=qq/(length_1*q[i]);
}

}

}

SEXP m_T22(SEXP MM2,SEXP kk,SEXP kk_2,SEXP qq,SEXP dim2){

double * M2=REAL(MM2);


int * dim=INTEGER(dim2);

SEXP ans_1;
ans_1=PROTECT(allocVector(REALSXP,(length(qq)+1)*length(kk)));

double * ans=REAL(ans_1);
double * s2= Calloc(length(qq)+1, double);
double * s3= Calloc(length(qq), double);

for(int r=0;r<length(kk);r++){

if(INTEGER(kk)[r]==1){for(int k=0;k<(length(qq)+1);k++)ans[r*(length(qq)+1)+k]=1;
}else{

for(int k=0;k<(length(qq)+1);k++)s2[k]=0;

for(int i=INTEGER(kk_2)[r];i<INTEGER(kk_2)[r+1];i++){

for(int j=i+1;j<INTEGER(kk_2)[r+1]; j++){

s2[0]+=corr__1(M2+i*dim[0],M2+j*dim[0],dim[0]);
quant__1(M2+i*dim[0],M2+j*dim[0],REAL(qq),dim[0],length(qq),s3);

for(int k=0;k<length(qq);k++)s2[k+1]+=s3[k];

}

}

for(int k=0;k<(length(qq)+1);k++)ans[r*(length(qq)+1)+k]=s2[k]*2/(INTEGER(kk)[r]*(INTEGER(kk)[r]-1));

};


for(int t=r+1;t<length(kk);t++){

for(int k=0;k<length(qq);k++)s2[k+1]=0;

for(int i=(INTEGER(kk_2)[r]);i<INTEGER(kk_2)[r+1];i++){

for(int j=(INTEGER(kk_2)[t]);j<INTEGER(kk_2)[t+1];j++){

s2[0]+=corr__1(M2+i*dim[0],M2+j*dim[0],dim[0]);
quant__1(M2+i*dim[0],M2+j*dim[0],REAL(qq),dim[0],length(qq),s3);

for(int k=0;k<length(qq);k++)s2[k+1]+=s3[k];

}}

for(int k=0;k<(length(qq)+1);k++){

ans[r*(length(qq)+1)+k]+=(s2[k]/(INTEGER(kk)[t]*INTEGER(kk)[r]));

};


}

for(int k=0;k<(length(qq)+1);k++)ans[r*(length(qq)+1)+k]/=(length(kk)-r);

}
Free(s2);Free(s3);
UNPROTECT(1);

return ans_1;

}

void R_init_mylib(DllInfo *info)
{

R_CallMethodDef callMethods[] = {
{"m_T22", (DL_FUNC) &m_T22, 5},{NULL, NULL, 0}
};

R_registerRoutines(info, NULL, callMethods, NULL, NULL);

}


