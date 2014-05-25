
/*
// c++ header files
#include <ctime.h>
#include <cmath.h>
#include <iostream.h>
#include <fstream.h>
*/
#include <iostream>
//#include <stdlib>
#include <map>
#include <vector>


// gsl header files
#include <gsl/gsl_combination.h>
//#include <gsl/gsl_rng.h>

/*
namespace RNG {
  gsl_rng * r;
}
*/

// namespaces
using namespace std;

int main(int argc, char *argv[]){
  unsigned long i,j,l,m;

  /*
  gsl_rng_env_setup();
  unsigned long genSeed = gsl_rng_default_seed;
  RNG::r = gsl_rng_alloc(gsl_rng_mt19937);
  
  gsl_rng_set(RNG::r, genSeed);
  */

  // my rudimentary weighted bipartite "network"
  vector<unsigned long> class_A_nodes;
  vector<unsigned long> class_B_nodes;
  vector<unsigned long> weights;

  // details for the matching
  map<unsigned long,unsigned long> class_A_matched;
  map<unsigned long,unsigned long> class_B_matched;
  map<unsigned long,unsigned long>::iterator m_iterator;

  // read in the network
  unsigned long a,b,w;
  while(cin >> a >> b >> w){
    class_A_nodes.push_back(a);
    class_B_nodes.push_back(b);
    weights.push_back(w);
  }

  // try combinations of what size?
  size_t n,k;
  n = weights.size();
  if(argc!=3){
    fprintf(stderr, "Incorrect number of arguments. Aborting...\n");
    return 1;
  }else{
    k = atoi(argv[1]);
    w = atoi(argv[2]);
  }
  
  // initialize the combination generator
  gsl_combination * c;
  c = gsl_combination_calloc (n, k);
  j = 0;

  double ww;
  bool failed;
  do
  {
    // assume that everything will work out
    failed = false;

    // designate all nodes as unmatched
    for(i=0;i<class_A_nodes.size();++i)
        class_A_matched[class_A_nodes[i]] = 0;
    for(i=0;i<class_B_nodes.size();++i)
        class_B_matched[class_B_nodes[i]] = 0;
    
    // start with zero weighting
    ww = 0;

    // sum up everything for the chosen combination
    for(i=0;i<k;i++){
        j = gsl_combination_get(c,i);
        ww += weights[j];
        class_A_matched[class_A_nodes[j]] += 1;
        if(class_A_matched[class_A_nodes[j]] > 1){
            failed = true;
            break;
        }
        class_B_matched[class_B_nodes[j]] += 1;
        if(class_B_matched[class_B_nodes[j]] > 1){
            failed = true;
            break;
        }
    }

    // if we didn't double match any nodes and we've got the target matched weight
    if(!failed && ww == w){
        // code to print out the details of the combination (i.e., which links were matched)
        printf ("{");
        //for(i = 0; i < k; i++)
        //    printf(" %zi", gsl_combination_get (c, i));
        gsl_combination_fprintf (stdout, c, " %u");
        printf (" }\n");
    }
  }
  while (gsl_combination_next (c) == GSL_SUCCESS);
  gsl_combination_free (c);

  // made it to the end; treat as a success
  return 0;
}

