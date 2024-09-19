#include "unirtrouver.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
//#include <bool.h>
#include <time.h>

const int AUCUN_MUR = 0;
const int MUR_EST = 1;
const int MUR_SUD = 1 << 1; // En binaire: 10
const int DEUX_MURS = MUR_EST | MUR_SUD; // En binaire: 11

int *init_labyrinthe(unsigned int taille){
	int* retour = (int*) malloc(taille*taille*sizeof(int)) ;
	for(int i=0; i< taille ; i++){
		for(int j=0; j<taille; j++){
			int sc = i*taille +j ; 
			retour[sc] = 3;
		}

	}
	return retour;
};

void afficher_labyrinthe(int *labyrinthe, unsigned int taille){
	for(int i=0; i<taille ; i++) printf(" _") ;
	printf("\n");
	for(int i=0; i<taille; i++){
		printf("|");
		for(int j=0; j<taille; j++){
		 	if(labyrinthe[i*taille+j]== 3) {
				printf("_|");
			}
			else if ((labyrinthe[i*taille+j]) == 2){
				printf("_ ");
			}
			else if((labyrinthe[i*taille+j]) == 1){
				printf(" |");
			}
			else{
				printf("  ");
			}
		}
		printf("\n");
	}
};

typedef struct{
	int a;
	int b;
} vec2;

struct mur {
	int der;//devant
	int dev;//derrière
} typedef mur_t;

mur_t *liste_murs(unsigned int taille){
	int taille_reelle = 2*(taille*(taille-1)) ; // maximum 2 murs par case, y a n^2 cases 
	mur_t* retour = malloc(taille_reelle*sizeof(mur_t)); //2 mur par case + bordure nord et ouest
//	for(int i=0 ; i <= 2*(taille*taille)-2*taille; i++ ){
//		int j = i - i%taille;
//		if(i%2 == 0) retour[i] = {{i%taille, j}, {i%taille + 1, j}} ;// on ajoute mur bas
//		if(i%2 == 1) retour[i] = {{i%taille, j}, {i%taille, j + 1}} ;//on ajoute mur droit
//	}
//	return retour;
	int compteur = 0;
	for(int i=0; i < taille ; i++){
		for(int j=0; j<taille ; j++){
			if(i != taille - 1){
				retour[compteur] = (mur_t) {i*taille +j, (i + 1)*taille + j };
				compteur ++ ;
				};
			if(j!= taille - 1){
				retour[compteur] = (mur_t) {i*taille + j, i*taille + j + 1} ;
				compteur ++; 
			};
		}
	}
	return retour;

}


void afficher_liste_murs(int taille, mur_t* lm) {
	int compteur = 0 ; 
	for(int i=0 ; i<2*(taille-1)*taille; i++){
			printf("{%d|%d}", lm[i].dev, lm[i].der);

	}

}

void permuter_murs(mur_t* liste_murs, int premier, int second){
	mur_t temp = liste_murs[premier] ; 
	liste_murs[premier] = liste_murs[second];
	liste_murs[second] = temp;
}


void melanger(mur_t *liste_murs, unsigned int taille){
	for(int i=0; i< taille ; i++){
			int k = rand() % (i+1) ; 
			permuter_murs(liste_murs, i, k);
	}
}

void construire(int *labyrinthe, unsigned int taille){
	int taille_reelle = 2*(taille * taille) ; 
	mur_t* lm = liste_murs(taille) ; 
	//afficher_liste_murs(lm, taille);
	melanger(lm,2*taille*(taille-1));
	element_t* partition = init_partition(taille*taille) ; // n^2 cases
	for(int i=0; i<2*taille*(taille-1); i++){
			element_t* ri = trouver (&partition[lm[i].der]) ; // représentant case de devant
			element_t* ri1 = trouver (&partition[lm[i].dev]); // représentant case de derrière
			//printf("\n %d-ème : der : %d ; dev : %d\n", i,ri->valeur, ri1->valeur);
			if(ri->valeur != ri1->valeur){
				printf("debug : lm[%d].dev - lm[%d].der = %d == 3\n",lm[i].dev, lm[i].der, lm[i].dev- lm[i].der);
				if(((lm[i].dev - lm[i].der) == 1)){
					unir(ri,ri1);
					labyrinthe[lm[i].der] -= 1 ;
					
				}
				else if(lm[i].dev - lm[i].der == taille) {
					unir(ri,ri1);
					labyrinthe[lm[i].der] -= 2 ;
				};
				afficher_labyrinthe(labyrinthe, taille);
 		 	};
		}
	print_partition(partition,taille*taille);
	//print_partition(partition, taille_reelle);
}
	


	

int main(int argc, char **argv) {
  // Initialisation du générateur de nombre aléatoire
  srand(time(NULL));

  if (argc < 2) {
    printf("Usage: ./unionfind_maze taille_labyrinthe\n");
    exit(1);
  }

  const unsigned int taille = atoi(argv[1]);
  printf("Construction d'un labyrinthe de taille %d x %d ...\n", taille, taille);

  int *labyrinthe = init_labyrinthe(taille);
//   int taille_reelle = 2*(taille*taille) ; 
//   mur_t* lm = liste_murs(taille) ; 
//   melanger(lm,taille);
//   element_t* partition = init_partition(taille*taille) ; 
//   element_t* r0 = trouver(&partition[lm[0].der]);
//   element_t* r1 = trouver(&partition[lm[0].dev]);
//   printf("Representant de %d : %d , de  %d : %d ", lm[0].der, r0->valeur, lm[0].dev, r1->valeur );
//   if(r0->valeur != r1->valeur){
// 	unir(r0,r1);
// 	if(lm[0].dev - lm[0].der == 1){
// 		labyrinthe[lm[0].der] -= 1 ;
// 	}
// 	else{
// 		labyrinthe[lm[0].dev] -= 2 ;
// 	}
//   };
//   print_partition(partition, taille*taille);






  //truc qui fait bugger en dessous
  //element_t* partition = init_partition(taille_reelle);

  construire(labyrinthe, taille);
  //print_partition(
  afficher_labyrinthe(labyrinthe, taille);
  // free(labyrinthe);
  return 0;
}
