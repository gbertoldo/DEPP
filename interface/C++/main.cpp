//    DEPP - Differential Evolution Parallel Program
//
//    Copyright (C) 2019 by Jonas Joacir Radtke, Guilherme Bertoldo and Carlos Henrique Marchi
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//    
//    Contact:
//          Jonas Joacir Radtke (a)
//                 E-mail: jonas.radtke@gmail.com
//
//          Guilherme Bertoldo (a)
//                 E-mail: glbertoldo@gmail.com
//
//          Carlos Henrique Marchi (b)
//                 E-mail: chmcfd@gmail.com
//    Institution
//          (a) Federal University of Technology - Paraná - UTFPR
//              Linha Santa Bárbara, s/n, Francisco Beltrão, Paraná, Brazil
//              Zip Code 85601-970
//              
//          (b) Federal University of Paraná - UFPR
//              Curitiba, Paraná, Brazil
//              Caixa postal 19040
//              Zip Code 81531-980
//

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
      
      // Setting output precision
      ofs.precision(16);

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
