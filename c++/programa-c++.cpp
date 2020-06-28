#include<iostream>
#include <cmath>
#include <vector>
using namespace std;
void mostrar(vector< vector<double> > A) {
    int filas =  A.size(); //filas
     int columnas = A[0].size(); //columnas
    for (int i=0; i<filas; i++) {
        //cout<<i+1<<" ";
        for (int j=0; j<columnas; j++) {
            /*if(j==columnas-1){printf("%F",A[i][j]);}else{
            printf("%F,",A[i][j]);}*/
            if(j==columnas-1){cout<<A[i][j];}else{
            cout<<A[i][j]<<",";}
        }
        cout << "\n";
    }
    cout << endl;
}
void mostrarC(vector<double>  A) {
    int filas =  A.size(); //filas
    for (int i=0; i<filas; i++) {
            printf("%F\t",A[i]);
    }
    cout << endl;
}
vector< vector<double> > inversa(vector< vector<double> > X) {
    int n =  X.size(); //filas
    vector<double> line(n,0);
    vector< vector<double> > B(n,line);
    vector<double> line2(n*2,0);
    vector< vector<double> > A(n,line2);
    for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
            A[i][j]=X[i][j];
        }
    }
    for (int i=0; i<n; i++) {
        A[i][n+i] = 1;
    }
    for (int i=0; i<n; i++) {
        double maxEl = abs(A[i][i]);
        int maxRow = i;
        for (int k=i+1; k<n; k++) {
            if (abs(A[k][i]) > maxEl) {
                maxEl = A[k][i];
                maxRow = k;
            }
        }

        for (int k=i; k<2*n;k++) {
            double tmp = A[maxRow][k];
            A[maxRow][k] = A[i][k];
            A[i][k] = tmp;
        }

        for (int k=i+1; k<n; k++) {
            double c = -A[k][i]/A[i][i];
            for (int j=i; j<2*n; j++) {
                if (i==j) {
                    A[k][j] = 0;
                } else {
                    A[k][j] += c * A[i][j];
                }
            }
        }
    }
    for (int i=n-1; i>=0; i--) {
        for (int k=n; k<2*n;k++) {
            A[i][k] /= A[i][i];
        }
        A[i][i] = 1; 

        for (int rowModify=i-1;rowModify>=0; rowModify--) {
            for (int columModify=n;columModify<2*n;columModify++) {
                A[rowModify][columModify] -= A[i][columModify] 
                                             * A[rowModify][i];
            }
            A[rowModify][i] = 0;
        }
    }
    for (int i=0; i<n; i++) {
        for (int j=n; j<n*2; j++) {
            B[i][j-n]=A[i][j];
        }
    }
    return B;
}
vector< vector<double> >  transpuesta(vector< vector<double> > A){
    int filas =  A.size(); //filas
    int columnas = A[0].size(); //columnas
    vector<double> line(filas,0);
    vector< vector<double> > B(columnas,line);
    for (int i=0; i<columnas; i++) {
        for (int j=0; j<filas; j++) {
            B[i][j]=A[j][i];
        }
    }
    return B;
}
vector< vector<double> > mult(vector< vector<double> > B,vector< vector<double> >A){
    float x=0;
     int filasB = B.size();//filas
     int columnasA = A[0].size(); //columnas
     int columnaBFilaA = B[0].size(); //columnas de la matriz B igual a las filas de la matriz A
    vector<double> line(columnasA,0);
    vector< vector<double> > X(filasB,line);
    for (int i=0; i<filasB; i++) {
        for(int k=0;k<columnasA;k++){
        x=0;
        for (int j=0; j<columnaBFilaA; j++) {
            x+=B[i][j]*A[j][k];
        }
        X[i][k]=x;
        }
    }
    return X;
}
vector< vector<double> > suma(vector< vector<double> > B,vector< vector<double> >A){
      int filas =  B.size(); //filas
     int columnas = B[0].size(); //columnas
    vector<double> line(columnas,0);
    vector< vector<double> > X(filas,line);
    for (int i=0; i<filas; i++) {
        for(int k=0;k<columnas;k++){
            X[i][k]=B[i][k]+A[i][k];
        }
    }
    return X;
}
vector< vector<double> > resta(vector< vector<double> > B,vector< vector<double> >A){
      int filas =  A.size();//filas
     int columnas = A[0].size(); //columnas
    vector<double> line(columnas,0);
    vector< vector<double> > X(filas,line);
    for (int i=0; i<filas; i++) {
        for(int k=0;k<columnas;k++){
            X[i][k]=B[i][k]-A[i][k];
        }
    }
    return X;
}
vector< vector<double> > DIVIDIR(vector< vector<double> > B,vector< vector<double> >A){
      int filas =  A.size();//filas
     int columnas = A[0].size(); //columnas
    vector<double> line(columnas,0);
    vector< vector<double> > X(filas,line);
    for (int i=0; i<filas; i++) {
        for(int k=0;k<columnas;k++){
            X[i][k]=B[i][k]/A[i][k];
        }
    }
    return X;
}
double media(vector< double> A){
     int n =  A.size();//filas
     double sum=0;
    for (int i=0; i<n; i++) {
        sum+=A[i];
    }
    return sum/n;
}
vector<vector<double>> llenar(int n,int m,double num){

    vector<double> line(m,0);
    vector< vector<double> > X(n,line);
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            X[i][j]=num;
        }
    }
    return X;
}
double varianza(vector< double> A){
      int n =  A.size();//filas
     double sum=0;
     double me=media(A);
    for (int i=0; i<n; i++) {
        sum+=((A[i]-me)*(A[i]-me));

    }
    return sum/(n-1);
}
double desviacion(vector< double> A){
    return sqrt(varianza(A));
}

vector<double> obtenerColumna(vector<vector<double>> X,int columna){
    int n=X.size();
    vector<double> r;
    for (int i=0; i<n; i++) {
        r.push_back(X[i][columna]); 
    }
    return r;
}
vector<vector<double>> obtenerColumnaMatriz(vector<vector<double>> X,int columna){
    int n=X.size();
    vector<double> line(1,0);
    vector<vector<double>> r(n,line);
    for (int i=0; i<n; i++) {
        r[i][0]=X[i][columna]; 
    }
    return r;
}
vector <vector<double>> SumaRestasCuadrado(vector< vector<double> > Y,vector< vector<double> >YP){
    int n=Y.size();
    int m=Y[0].size();
    vector<double> line(m,0);
    vector<vector<double>> r(1,line);
    vector<vector<double>> x;
    x=resta(Y,YP);
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            r[0][j]+=(x[i][j]*x[i][j]);
        }
    }
    /*for(int j=0;j<m;j++){
        r[0][j]/=n;
    }*/
    return r;
}
vector<double> multEscalar(double escalar,vector<double > X){
    for(int k=1;k<X.size();k++){
            X[k]=escalar*X[k]; //leemos la matriz x
    }
    return X;
}
vector< vector<double> > regresionSinIntercepto(vector< vector<double> > X,vector< vector<double> >Y){
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return mult(inversa(mult(transpuesta(X),X)),mult(transpuesta(X),Y));  
}
vector< vector<double> > regresion(vector< vector<double> > X,vector< vector<double> >Y){
    int n=X.size();
    int m=X[0].size()+1;
    vector<double> line(m,0);
    vector<vector<double>> Xcompleta(n,line);
    //COMPLETAMOS LA MATRIZ X CON LA COLUMNA DE UNOS
    for (int i=0; i<n; i++) {
        Xcompleta[i][0]=1; // columna de 1's correspondiente al intercepto
        for(int k=1;k<m;k++){
            Xcompleta[i][k]=X[i][k-1]; //leemos la matriz x
        }
    }
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return mult(inversa(mult(transpuesta(Xcompleta),Xcompleta)),mult(transpuesta(Xcompleta),Y));
}
bool hipotesisRegresor(double tcalc,double Ttabla){
        if(abs(tcalc)<Ttabla){
          return true;
        }else{
           return false;
        }
}

vector< vector<double> > lag(vector< vector<double> > X,int i){
    int n=X.size()-i;
    int m=i;
    vector<double> line(m,0);
    vector<vector<double>> Xlag(n,line);
    for (int j=0; j<n; j++) {
        for(int k=0;k<m;k++){
            Xlag[j][m-1-k]=X[j+k][0];
        }
    }
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return Xlag;
}
vector< vector<double> > quitar(vector< vector<double> > X,int x){
    int n=X.size();
    int m=X[0].size();
    vector<double> line(m,0);
    vector<vector<double>> Y(n-x,line);
    for (int j=x; j<=n-1; j++) {
        for(int i=0;i<m;i++){
        Y[j-x][i]=X[j][i];
        }
    }
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return Y;
}
vector< vector<double> > desintegrar(vector< vector<double> > X){
    int n=X.size()-1;
    vector<double> line(1,0);
    vector<vector<double>> Y(n,line);
    for (int j=0; j<n; j++) {
        Y[j][0]=X[j+1][0]-X[j][0];
    }
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return Y;
}
vector< vector<double> > integrar(vector< vector<double> > X){
    int n=X.size();
    vector<double> datosPredichos=obtenerColumna(X,0);
    vector<double> line(1,0);
    vector<vector<double>> Y(datosPredichos.size(),line);
    Y[0][0]=0+datosPredichos[0];
    for (int j=1; j<n; j++) {
        //cout<<datosPredichos[j]<<endl;
        Y[j][0]=datosPredichos[j]+Y[j-1][0];
    }
    //FORMA MATRICIAL REGRESION LINEAL MULTIPLE
    return Y;
}
vector<vector<double>> merge(vector<vector<double>> X,vector<vector<double>> Y){
        int n=X.size();
        int m=X[0].size();
        vector<double> line(m+1,0);
        vector<vector<double>> r(n,line);
        for(int i=0;i<n;i++){
            r[i][0]=Y[i][0];
        }
        for (int j=0; j<n; j++) {
            for(int k=1;k<m+1;k++){
                r[j][k]=X[j][k-1];
        }
    }
    return r;
}
vector<vector<double>>  predecir(vector<vector<double>> M,vector<vector<double>> r,int x){
        int n=M.size();
        int m=M[0].size();
        double res=0;
        vector<double> line(m,0);
        vector<vector<double>> R(n+x,line);
        for (int j=0; j<n; j++) {
            for(int k=0;k<m;k++){
                R[j][k]=M[j][k];
            }
        }
        for (int j=n; j<n+x; j++) {
            int k=0;
            res=0;
            for(k=0;k<m-1;k++){
                R[j][k+1]=R[j-1][k];
                res+=R[j][k+1]*r[k][0];
            }
            R[j][0]=res;
        }
    return R;
}
vector <vector<double>> medias(vector<vector<double>> Y){
    int n=Y.size();
    int m=Y[0].size();
    vector<double> med;
    for(int k=0;k<m;k++){
        med.push_back(media(obtenerColumna(Y,k)));
    }
    for (int j=0; j<n; j++) {
        for(int k=0;k<m;k++){
            Y[j][k]=med[k];
        }
    }
    return Y;
}
int main(){
    //SIMPLEMENTE DESCOMENTAR EL AMBITO DESEADO (ENTRADA-SALIDA) Y COMENTAR EL AMBITO ACTUAL(ENTRADA-SALIDA) 
    int n,m,T,z;   
    freopen ("salud.txt", "r", stdin);
    //freopen ("social.txt", "r", stdin);
    //freopen ("consumo.txt", "r", stdin);

    //IMPRIMIMOS LA SALIDA EN UN ARCHIVO TXT
    freopen ("nivel-salud.txt", "w", stdout);
    //freopen ("puntaje-social.txt", "w", stdout);
    //freopen ("puntaje-consumo.txt", "w", stdout);
    cin>>z>>n>>m>>T;// leemos el tamano de la muestra n y numero total de variables(dependientes+independientes) m
    vector<double> line(z,0);
    vector< vector<double> > Y(T,line);
    vector<double> line2(n,0);
    vector< vector<double> > X(T,line2);
    vector<double> line3(m,0);
    vector< vector<double> > test(T,line3);
    vector< vector<double> > predicciones,regresores;
    //DIVIDIMOS  LOS DATOS EN LA MATRIZ DE ENTRENAMIENTO,PRUEBA, LABEL
    for(int t=0;t<T;t++){
        for (int i=0; i<z; i++) {
            cin>>Y[t][i];
        }
        for (int i=0; i<n; i++) {
            cin>>X[t][i];
        }
        for (int i=0; i<m; i++) {
            cin>>test[t][i];
        }
    }
    regresores=regresionSinIntercepto(X,Y);
    predicciones=mult(test,regresores);
    cout<<"Regresores\n";
    mostrar(transpuesta(regresores));
    cout<<"Predicciones\n";
    mostrar(predicciones);
    return 0;
}
