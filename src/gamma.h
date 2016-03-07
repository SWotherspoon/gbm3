//------------------------------------------------------------------------------
//
//  File:       gamma.h
//
//  Description: gamma distribution
//
//------------------------------------------------------------------------------

#ifndef __gamma_h__
#define __gamma_h__

//------------------------------
// Includes
//------------------------------
#include "distribution.h"
#include "dataset.h"
#include <Rmath.h>
#include <memory>

//------------------------------
// Class definition
//------------------------------
class CGamma : public CDistribution
{

public:

	//---------------------
	// Factory Function
	//---------------------
	static CDistribution* Create(SEXP radMisc, const CDataset& data,
											const char* szIRMeasure,
											int& cGroups, int& cTrain);

    //---------------------
    // Public destructor
    //---------------------
    virtual ~CGamma();

    //---------------------
    // Public Functions
    //---------------------
    void ComputeWorkingResponse(const double *adF,
				double *adZ,
				const bag& afInBag,
				unsigned long nTrain);

    void InitF(double &dInitF,
	       unsigned long cLength);
    
    void FitBestConstant(const double *adF,
			 double *adZ,
			 const std::vector<unsigned long>& aiNodeAssign,
			 unsigned long nTrain,
			 VEC_P_NODETERMINAL vecpTermNodes,
			 unsigned long cTermNodes,
			 unsigned long cMinObsInNode,
			 const bag& afInBag,
			 const double *adFadj);
    
    double Deviance(const double *adF,
                    unsigned long cLength,
                    bool isValidationSet=false);

    double BagImprovement(const double *adF,
                          const double *adFadj,
                          const bag& afInBag,
                          double dStepSize,
                          unsigned long nTrain);
private:
    //----------------------
    // Private Constructors
    //----------------------
    CGamma(SEXP radMisc, const CDataset& data);

	//-------------------
	// Private Variables
	//-------------------
    vector<double> vecdNum;
    vector<double> vecdDen;
    vector<double> vecdMax;
    vector<double> vecdMin;
};

#endif // __gamma_h__



