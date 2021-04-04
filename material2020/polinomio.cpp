#include <bits/stdc++.h>

using namespace std;

int main(){
	int n;
	cin >> n;
	int coeficientes[n+1];
	int T[n+1];
	int polinomios[n+1];
	for (int i=0; i<=n; i++){
		int coeficiente;
		cin >> coeficiente;
		 coeficientes[i] = coeficiente;
		 cout << coeficientes[i] << ' ';
	}

	for (int i = 0; i <= n;i++){
		T[i] = coeficientes[i];
	}
	for (int i = 1; i < n; i++)
		{
			polinomio[i] = polinomio[i-1] + T[1];
			for (int i =1; i <= n-1;i++ ){

				T[i] = T[i] + T[i+1];
			}
		}	
	for (int i = 0; i <=n ; i++)
	{
		cout << polinomio[i] <<" ";
	}
			

	return 0;
}