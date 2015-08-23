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
#include <string>
#ifndef _FINDCOMBBYIDX_H_
#define _FINDCOMBBYIDX_H_


namespace stdcomb
{

// template class
template< typename CFTC, typename N >
class CFindCombByIdx
{

public:
	CFindCombByIdx() {};
	~CFindCombByIdx() {};


	bool FindCombByIdx( 
			const unsigned int Set, 
			const unsigned int Comb, 
			N Num, 
			std::vector<unsigned int> &vi )
	{
		if( Comb > Set || Set == 0 || Comb == 0 )
			return false;
			
		if( Num == 0 )
		{
			for( unsigned int i=0; i<Comb; ++i )
				vi[i] = i;
				
			return true;
		}
		
		// Actual Processing
		unsigned int RSet = Set - 1;    // Remaining Set
		unsigned int RComb = Comb - 1;  // Remaining Comb
		CFTC ToFind(Num);
		CFTC Zero(0);
		CFTC One(1);

		for( unsigned int x=0; x<Comb; ++x )
		{
			
			if( x == Comb-1 ) // Last Element
			{
				while( true )
				{
					if( Zero == ToFind )
					{
						vi[x] = Set - RSet - 1;
						break;			
					}
					else
					{
						ToFind -= One;
						--RSet;
					}
				}
			}
			else
			{
				CFTC ftc;
				CFTC Prev;
				
				unsigned int yLoop = RSet-RComb;
				bool Found = false;
				unsigned int xPrev = 0;

				if( x > 0 )
					xPrev = vi[ x-1 ] + 1;

				unsigned int y;
				for( y=0; y<yLoop; ++y )
				{
					ftc.FindTotalComb(RSet, RComb);
					ftc += Prev;
					if( ftc > ToFind ) // Prev is the found one
					{
						ToFind -= Prev;
											
						vi[x] = y + xPrev;

						Found = true;
						break;
					}
					Prev = ftc;
					--RSet;
				}

				if( !Found )
				{
					ToFind -= ftc;
					
					vi[x] = y + xPrev;

				}
				
				--RSet;
				--RComb;
			}
		}
		
		return true;
	};
	// find the position using the combination elements
	bool FindIdxByComb( 
		const unsigned int Set, 
		const unsigned int Comb, 
		N& Num, 
		std::vector<unsigned int> &vi )
	{
		if( Comb > Set || Set == 0 || Comb == 0 )
			return false;

		// Actual Processing
		unsigned int RSet = Set - 1;    // Remaining Set
		unsigned int RComb = Comb - 1;  // Remaining Comb
		CFTC Pos(0);
		for( unsigned int x=0; x<Comb; ++x )
		{
			CFTC ftc(0);
			CFTC Prev(0);

			unsigned int yLoop = RSet-RComb;

			unsigned int y;
			for( y=0; y<yLoop+1; ++y )
			{
				ftc.FindTotalComb(RSet, RComb); // compute the total combinations for this set and comb elements
				ftc += Prev; // add back the saved value to the current value
				if( vi[x] == Set-RSet-1 ) // if this element is the same value, eg 8==8
				{
					Pos += Prev; // add the prev value to the position
					break; // do not compute anymore.
				}
				Prev = ftc; // save ftc value
				--RSet;
			}
			--RSet;
			--RComb;
		}

		Num = Pos.GetResult(); // convert the result to BigInteger

		return true;
	};

};


} // stdcomb

#endif // _FINDCOMBBYIDX_H_