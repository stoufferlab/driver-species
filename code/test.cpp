#include <iostream>
#include <map>
#include <vector>
#include "IndexCombination.h"
#include "FindCombByIdx.h"
#include "FindTotalComb.h"
#include "IndexCombination.h"
#include "BigIntegerLibrary.h"
#include "combination.h"
#include <gsl/gsl_combination.h>

using namespace std;
using namespace stdcomb;

int main(int argc, char *argv[]){
	unsigned long i,j,l,m;

	// my rudimentary weighted bipartite "network"
	vector<int> class_A_nodes;
	vector<int> class_B_nodes;
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

	std::cout << "\n" << "class A Nodes: ";
	for (vector<int>::iterator i = class_A_nodes.begin();
      i != class_A_nodes.end(); ++i)
		std::cout << ' ' << *i ;
	std::cout << "\n" << "\n";

	std::cout << "class B Nodes: ";
	for (vector<int>::iterator i = class_B_nodes.begin();
      i != class_B_nodes.end(); ++i)
		std::cout << ' ' << *i ;
	std:cout << "'\n" << "\n";

	std::cout << "weights: ";
	for (vector<unsigned long>::iterator i = weights.begin();
      i != weights.end(); ++i)
		std::cout << ' ' << *i;
	std::cout << "\n" << "\n";


	// try combinations of what size?
	std::size_t n;  // total number of links
	std::size_t k;  // desired matching size
	std::size_t o;  // block number (for parallel computation)

	n = weights.size();
	if(argc!=3){
		fprintf(stderr, "Incorrect number of arguments. Aborting...\n");
		return 1;
	}else{
		k = atoi(argv[1]);  // desired matching size
		w = atoi(argv[2]);  // matching weight
	}

	// initialize the combination generator
	gsl_combination * c;
	c = gsl_combination_calloc (n, k);
	j = 0;

	size_t max_size = (size_t)-1;
	std::cout << "\n" << "Maximum size of size_t: " << max_size << "\n" ;

	CFindCombByIdx< CFindTotalComb<BigInteger>, BigInteger > findcomb;
	const unsigned int nComb = 3;
	const unsigned int nSet = 10;
	vector<unsigned int> vec(nComb);

	for(size_t i=0; i<120; ++i)
	{
		findcomb.FindCombByIdx( nSet, nComb, i, vec );

				cout<< vec.at(0) << ",";
				cout<< vec.at(1) << ",";
				cout<< vec.at(2) << ",";

				BigInteger num(0);
				findcomb.FindIdxByComb( nSet, nComb, num, vec );
				cout<< "Pos:"<<num << endl;

				if(num!=i)
				{
					cout<< "Error!!!"<< endl;
					break;
				}

	}

	/*
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
			printf("{");
			//for(i = 0; i < k; i++)
			//    printf(" %zi", gsl_combination_get (c, i));
			gsl_combination_fprintf (stdout, c, " %u");
			printf(" }\n");
		}
	}
	while (gsl_combination_next (c) == GSL_SUCCESS);
	gsl_combination_free (c);
	 */

	// made it to the end; treat as a success
	return 0;
}
