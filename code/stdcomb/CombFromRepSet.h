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

#include <vector>
#include "IndexCombination.h"

#ifndef _COMBFROMREPSET_H_
#define _COMBFROMREPSET_H_


namespace stdcomb
{


struct RepElem 
{
	unsigned int Start;
	unsigned int End;
};


class CCombFromRepSet : public CIdxComb
{
public:
	// Constructor
	CCombFromRepSet() {};

	CCombFromRepSet( unsigned int SetSize, unsigned int CombSize )
	: CIdxComb( SetSize, CombSize )
	{};
	
	~CCombFromRepSet() {};
	
	bool SetRepeatSetInfo( const std::vector<unsigned int> &vi );

	// Getvrep() must be removed
	//============================
	std::vector<RepElem> & Getvrep()
	{
		return m_vrep;
	};
	
	void SetFirstComb( std::vector<unsigned int> &vi );

	bool GetNextComb( std::vector<unsigned int> &vi );
	
private:
	void Shift( std::vector<unsigned int> &vi );

	std::vector<RepElem> m_vrep;

};


}

#endif // _COMBFROMREPSET_H_