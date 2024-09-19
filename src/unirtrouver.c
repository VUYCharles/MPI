#include "unirtrouver.h"

#include <stdio.h>
#include <stdlib.h>

void init(element_t *element, int valeur) {
  element->valeur = valeur;
  element->parent = element;
  element->rang = 0;
} 

element_t *init_partition(unsigned int nb_elements) {
  element_t *elements = malloc(nb_elements * sizeof(element_t));
  if (!elements) {
    perror("init_partition");
    exit(1);
  }
  for (unsigned int i = 0; i < nb_elements; i++) {
    init(&(elements[i]), i);
  }
  return elements;
}

void free_partition(element_t *elements) {
  free(elements);
}

void lier(element_t *premier, element_t *second) {
  // Si le second est moins
  if (premier->rang > second->rang) {
    second->parent = premier;
  }
  else {
    premier->parent = second;
    if (premier->rang == second->rang) {
      second->rang += 1;
    }
  }
}

element_t *trouver(element_t *element) {
  if (element->parent != element) {
    element->parent = trouver(element->parent);
  }
  return element->parent;
}

element_t* trouver_iterative(element_t* element){
	//element* elcourant = (element*) malloc(sizeof(element)) ; 
	element_t* el_courant = element ; 
	while(el_courant != el_courant->parent){
		el_courant = el_courant->parent; 
	}
	//on rajoute la compression de chemin
//while(element->parent != parent) {
//		element_t* temp = element->parent; 
//		element->parent = el_courant;
//		element = next 
//	} pas nécessaire
//	on le met sous son représentant
	element->parent = el_courant;
	return el_courant;
}

void unir(element_t *premier, element_t *second) {
  lier(trouver(premier), trouver(second));
}

void print_partition(element_t *partition, unsigned int nb_elements) {
  for (unsigned int i = 0; i < nb_elements; i++) {
    printf("%d [%d] -> %d\n", partition[i].valeur, partition[i].rang, partition[i].parent->valeur);
  }
}
