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


#include "CombFromRepSet.h"

using namespace stdcomb;

bool CCombFromRepSet::SetRepeatSetInfo( const std::vector<unsigned int> &vi )
{
	if( vi.size() == 0 )
		return false;
	
	// empty vrep!
	m_vrep.clear();
			
	RepElem re;
	re.Start = 0;
	unsigned int CurrElem = vi[0];
	bool bRepeat = false;
	for( unsigned int i=1; i<vi.size(); ++i )
	{
		if( bRepeat == false && CurrElem == vi[i] )
		{
			bRepeat = true;
			if( i == vi.size()-1 ) // last element
			{
				re.End = i;
				m_vrep.push_back( re );
			}
			continue;
		}
		
		if( bRepeat == false && CurrElem != vi[i] )
		{
			CurrElem = vi[i];
			re.Start = i;
			continue;
		}

		if( bRepeat == true && CurrElem == vi[i] )
		{
			if( i == vi.size()-1 ) // last element
			{
				re.End = i;
				m_vrep.push_back( re );
			}
			
			continue;
		}
		
		if( bRepeat == true && CurrElem != vi[i] )
		{
			CurrElem = vi[i];

			re.End = i - 1;
			m_vrep.push_back( re );
			
			re.Start = i;
			
			bRepeat = false;
		}
	}

	return true;
}


void CCombFromRepSet::SetFirstComb( std::vector<unsigned int> &vi )
{
	Shift( vi );
	
}


bool CCombFromRepSet::GetNextComb( std::vector<unsigned int> &vi )
{
	if( !CIdxComb::GetNextComb( vi ) )
		return false;

	Shift( vi );
	
	return true;	
}


void CCombFromRepSet::Shift( std::vector<unsigned int> &vi )
{
	if( m_vrep.size() == 0 || vi.size() == 0 )
		return;

	for( unsigned int i=m_vrep.size()-1; i>=0 ; --i )
	{
		unsigned int RepCnt = 0;
		for( int j=vi.size()-1; j>=0; --j )
		{
			if( vi[j] >= m_vrep[i].Start && vi[j] <= m_vrep[i].End )
			{
				vi[j] = m_vrep[i].End - RepCnt;
				
				++RepCnt;

			}

			if( j==0 )
				break;
		}

		if( i==0 )
			break;
	}

}
