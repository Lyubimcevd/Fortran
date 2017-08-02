#include <fstream>
#include <string>
#include<iostream>
#include <conio.h>

using namespace std;

void main() 
{
	int a1,a2,k1 = 1,k2 = 1;
	ifstream creat1,creat2;
	creat1.open("tmp1.txt");
	creat2.open("TMP2.TXT");
	while (!creat1.eof()&&!creat2.eof())
	{
		if (k1==8)
		{
			k1 = 1; 
			k2++;
		}
		k1++;
		creat1 >> a1;
		creat2 >> a2;
		cout<<a1<<"   "<<a2<<"\n";
		if (a1!=a2) 
		{
			cout<<"Error: "<<a1<<"   "<<a2<<"   "<<k1<<"   "<<k2;
			getch();
			return;
		}
	}
	cout<<"Not error";
	getch();
}