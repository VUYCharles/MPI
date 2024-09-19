#include <stdio.h>
#include <stdlib.h>

typedef struct {
	int* s; //sommets 
	int* r;	//rangs
	int n;	//taille
} partition ; 

partition* init(int n){
	partition* re = malloc(sizeof(partition));
	re->s = calloc(n,sizeof(int));
	re->r = calloc(n,sizeof(int));
	re->n = n ; 
	for(int i=0 ; i<n ; i++){
		re->s[i] = i ; 
	}
	return re;
}

int trouver_cc(partition* p, int parent){
	//int parent = p->s[a] ; 
	while(parent != p->s[parent]){
		p->s[parent] = trouver_cc(p, p->s[parent]) ;
		parent = p->s[parent];
	}
	return p->s[parent] ;
}

void unir(partition* p, int a, int b){
	int ra = trouver_cc(p,a) ; 
	int rb = trouver_cc(p,b) ; 
	if(p->r[ra] == p->r[rb]) {
		p->s[rb] = ra ;
		p->r[ra] ++ ; 
	}
	else if(p->r[ra] > p->r[rb]){ 
		p->s[rb] = ra ;
	}
	else {
		p->s[ra] = rb ;
	}
	return ;
}	

void disp(partition* p){
	for(int i=0 ; i< p->n; i++){
		printf("(%d|%d)", p->s[i], p->r[i]);
	}
	printf("\n");
}


void test_cours(){
	partition* p = init(6) ; 
	unir(p,0,4);
	disp(p);
	unir(p,0,5);
	disp(p);
	unir(p,3,0);
	disp(p);
	int a = trouver_cc(p,5);
	disp(p);
}

int main(){
	test_cours();
	return 0;
}
