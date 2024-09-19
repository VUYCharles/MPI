#ifndef UNIONFIND_H
#define UNIONFIND_H

struct element {
  int valeur;
  struct element* parent;
  int rang; // Borne supérieure sur la hauteur du sous-arbre
            // dont l'élément est la racine
} typedef element_t;

void init(element_t *element, int valeur);

element_t *init_partition(unsigned nb_elements);

void free_partition(element_t *elements);

void lier(element_t *premier, element_t *second);

element_t *trouver(element_t *element);

void unir(element_t *first, element_t *second);

void print_partition(element_t *partition, unsigned int nb_elements);

#endif // UNIONFIND_H