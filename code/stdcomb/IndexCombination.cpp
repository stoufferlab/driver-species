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

#include "IndexCombination.h"

using namespace stdcomb;


void CIdxComb::Init( unsigned int SetSize, unsigned int CombSize  )
{
	// Assign CombSize
	////////////////////////
	if( CombSize == 0 )
		CombSize = 1;

	m_ArrSize = CombSize;
	m_LastIdx = CombSize - 1;

	// Assign SetSize
	////////////////////////
	if( SetSize == 0 )
		SetSize = 2;

	if( CombSize > SetSize )
		CombSize = SetSize;
	
	m_SetSize = SetSize;
	m_LastSetIdx = SetSize - 1;
}


bool CIdxComb::SetSizes( unsigned int SetSize, unsigned int CombSize )
{
	if( SetSize == 0 )
		return false;

	if( CombSize == 0 )
		return false;

	if( CombSize > SetSize )
		return false;
	
	m_ArrSize = CombSize;
	m_LastIdx = CombSize - 1;

	m_SetSize = SetSize;
	m_LastSetIdx = SetSize - 1;

	return true;

}


bool CIdxComb::GetNextComb( std::vector<unsigned int> &vi )
{
	
	// Check if the last element is at the end
	if( vi[m_LastIdx] == m_LastSetIdx )
	{
		if( m_ArrSize == 1 ) // Completed
			return false;

		// Check if the subsequent elements(counted from back)
		// is also at their subsequent positions
		//////////////////////////////////////////////////////
		bool Completed = true;
		// Incomplete Index, init value not used
		unsigned int IncompIdx = m_LastIdx - 1; 
		
		bool FirstIdx = false;
		unsigned int ArrIdx = m_LastIdx - 1;

		unsigned int SetIdx = m_LastSetIdx - 1;
		
		while( !FirstIdx )
		{
			if( vi[ArrIdx] != SetIdx )
			{
				Completed = false;
				IncompIdx = vi[ArrIdx] + 1;
				break;
			}

			if( SetIdx )
				--SetIdx;
		
			if( !ArrIdx )
				FirstIdx = true;
			else
				--ArrIdx; 
				 
		}

		if( Completed )
			return false;
		else
		{
			for( unsigned int i=ArrIdx; i<=m_LastIdx; ++i, ++IncompIdx )
			{
				vi[i] = IncompIdx;
			}
		}
	
	}
	else if ( vi[m_LastIdx] < m_LastSetIdx )
	{
		(vi[m_LastIdx])++;
	}
	else // bigger than the m_LastIdx! Impossible!
	{
		return false;
	} 
	
	return true;
}


