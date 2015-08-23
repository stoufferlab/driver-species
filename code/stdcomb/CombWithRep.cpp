/*
MIT License

Combination Library version 1.5

Copyright (c) 2007 Wong Shao Voon

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/


#include "CombWithRep.h"




// iterative
bool stdcomb::CombWithRep( 
	unsigned int Set, 
	unsigned int Comb, 
	std::vector<unsigned int> &vi )
{
	if( Set == 0 || Comb == 0 )
		return false;
	
	bool bEndReach = false;
	for( int x=Comb-1; x>=0 ; --x )
	{
		
		if( x == 0 && vi[x] == Set - 1 )
			return false;

		if( bEndReach )
		{

			if( vi[x] != Set - 1 )
			{
				unsigned int level = vi[x] + 1;
				for( unsigned int y=x; y<Comb; ++y )
					vi[y] = level;
					
				return true;
			}		
		}
		
		// At the end of the Set
		if( vi[x] == Set - 1 )
		{
			bEndReach = true;
		}
		else if( vi[x] < Set - 1 )
		{
			(vi[x])++;
			return true;		
		}
	}

	return true;		

}
