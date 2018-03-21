#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>


using namespace std;


class Depp_Interface{

private:

   string ofile;

public:

   // Given the input file from DEPP (ifile), returns the vector of parameters x.
   void get_parameters(const string& ifile, vector<double>& x){

      int            nu; // Number of Unknowns
      string        str; // Temporary string
      istringstream  ss; // Temporary string stream
      
      
      // Openning the input file
      ifstream ifs(ifile.c_str());

      // Reading the name of the output file
      getline(ifs,str); ss.clear(); ss.str(str); ss >> ofile;
      
      // Removing apostrophe chars
      ofile.erase(remove(ofile.begin(), ofile.end(), '\''), ofile.end());
      
      // Reading the number of unknowns
      getline(ifs,str); ss.clear(); ss.str(str); ss >> nu;
         
      // Reading the x vector
      double daux;

      for (int i = 0; i<nu; ++i){

         getline(ifs,str); ss.clear(); ss.str(str); ss >> daux;

         x.push_back(daux);
         
      }

      ifs.close();

      return;

   }


   // Saves the fitness and the exit status to the DEPP output file
   void save_fitness(const double& fit, const int& estatus){

      // Openning the output file
      ofstream ofs(ofile.c_str());

      // Writing the fitness
      ofs << fit << " = Fitness" << endl;

      // Writing the exit status
      ofs << estatus << " = Exit status ( 0 = success, 1 = failure, 2 = generate another individual)" << endl;

      ofs.close();

      return;

   }

};



// Sphere function (Coley, page 38, 1999). An example of fitness function.
double function1(const vector<double>& x){

   double daux = 79.0;

   for (const double& x_i: x){
     
      daux = daux - x_i * x_i;

   }

   return daux;

}


Depp_Interface depp_interface;


int main(int argc, char *argv[])
{

   vector<double> x; // Vector of parameters


   // Reading the vector of parameters x
   depp_interface.get_parameters(string(argv[1]), x);

   // Calculating the fitness function
  double fit = function1(x);

   // Saving the result
   depp_interface.save_fitness(fit, 0);

   return 0;
}
