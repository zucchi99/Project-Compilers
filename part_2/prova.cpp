#include <iostream>
#include "Configuration.hpp"

#define my_pair std::pair<Value, std::list<Value*>>

int main() {
    Configuration s;
    std::list<Value*> l;

    std::map<std::string, my_pair> assignments_s1 = {
        { "a", my_pair(Value("ciao"), l) }, 
        { "b", my_pair(Value(1),      l) }, 
        { "c", my_pair(Value(true),   l) }
    };
    s.insert_empty_section("sezione1");
    s.modify_assignments_to_section("sezione1", assignments_s1);

    std::map<std::string, my_pair> assignments_s2 = {
        { "d", my_pair(Value("$sezione1.a", NULL), l) }, 
        { "e", my_pair(Value("$sezione2.d", NULL), l) }, 
        { "f", my_pair(Value("$sezione3.g", NULL), l) }, 
    };
    s.insert_empty_section("sezione2");
    s.modify_assignments_to_section("sezione2", assignments_s2);
    
    std::map<std::string, my_pair> assignments_s3 = {
        { "g", my_pair(Value("$sezione2.e", NULL), l) }, 
        { "h", my_pair(Value("$sezione2.f", NULL), l) }, 
        { "i", my_pair(Value("$sezione3.h", NULL), l) }, 
    };
    s.insert_empty_section("sezione3");
    s.modify_assignments_to_section("sezione3", assignments_s3);

    s.add_pointers_references();

    std::cout << s << std::endl;
    //std::cout << s.to_String(true) << std::endl;

    std::cout << "--------------------------------------------------" << std::endl;


    std::cout << "s.delete_section(\"sezione2\")" << std::endl;
    s.delete_section("sezione2");  

    std::cout << s << std::endl;
    //std::cout << s.to_String(true) << std::endl;

    std::cout << "--------------------------------------------------" << std::endl;

    return 0;
}


int main_2() {
    Configuration s;
    std::list<Value*> l;
    auto assignment1 = my_pair(Value("ciao"), l); 
    auto assignment2 = my_pair(Value(1), l); 
    auto assignment3 = my_pair(Value(true), l); 
    auto assignment4 = my_pair(Value("$sezione2.c", NULL), l);
    auto assignment5 = my_pair(Value("$sezione1.a", NULL), l);
    auto assignment6 = my_pair(Value("$sezione1.d", NULL), l);
    auto assignment7 = my_pair(Value("$sezione1.e", NULL), l);

    std::map<std::string, my_pair> assignments = {
        { "a", assignment1 }, 
        { "b", assignment2 }, 
        { "c", assignment3 }, 
        { "d", assignment4 }, 
        { "e", assignment6 }, 
        { "f", assignment7 }
    };

    std::list<Value*> temp_empty_list;
    auto assignment1 = std::pair<Value, std::list<Value*>>(Value("ciao"), temp_empty_list); 
    auto assignment2 = std::pair<Value, std::list<Value*>>(Value(1), temp_empty_list); 
    auto assignment3 = std::pair<Value, std::list<Value*>>(Value(true), temp_empty_list); 
    auto assignment4 = std::pair<Value, std::list<Value*>>(Value("$sezione2.c", NULL), temp_empty_list);
    auto assignment5 = std::pair<Value, std::list<Value*>>(Value("$sezione1.a", NULL), temp_empty_list);
    auto assignment6 = std::pair<Value, std::list<Value*>>(Value("$sezione1.d", NULL), temp_empty_list);
    auto assignment7 = std::pair<Value, std::list<Value*>>(Value("$e", NULL), temp_empty_list);

   std::map<std::string, std::pair<Value, std::list<Value*>>> assignments = {{ "a", assignment1 }, { "b", assignment2 }, { "c", assignment3 }, { "d", assignment4 }, { "e", assignment6 }, { "f", assignment7 }};
    
    s.insert_empty_section("sezione1");
    
    s.insert_empty_section("sezione2");

    s.modify_assignments_to_section("sezione1", assignments);

    assignments = { { "a", assignment5 }, { "b", assignment1 }, { "c", assignment2 } };

    s.modify_assignments_to_section("sezione2", assignments);

    s.add_pointers_references();

    std::cout << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;

    // Caso1
    s.delete_assignment("sezione1", "d");
    std::cout << "Caso1: A -> B -> C, cancello B, aggiorno puntatore di A da B a C, nella backlist di C metto A al posto di B" << std::endl;
    std::cout << "Caso 1, delete sezione1 d " << std::endl << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;
    // Caso2
    s.delete_assignment("sezione1", "a");
    std::cout << "Caso2: A -> B -> C, cancello C, cambio il Value (che è di tipo Pointer) di B con quello che aveva C" << std::endl;
    std::cout << "Caso 2, delete sezione1 a " << std::endl << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;
    // Caso1
    s.delete_assignment("sezione1", "e");
    std::cout << "Caso1: A -> B -> C, cancello B, aggiorno puntatore di A da B a C, nella backlist di C metto A al posto di B" << std::endl;
    std::cout << "Caso 1, delete sezione1 e " << std::endl << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;
    // Caso3
    s.delete_assignment("sezione1", "f");
    std::cout << "Caso3: A -> B -> C, cancello A, cancello nella backlist di B il puntatore ad A" << std::endl;
    std::cout << "Caso 3, delete sezione1 f " << std::endl << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;
    // Caso4
    s.delete_assignment("sezione1", "b");
    std::cout << "Caso4: A, cancello A" << std::endl;
    std::cout << "Caso 4, delete sezione1 b " << std::endl << s << std::endl;
    std::cout << "--------------------------------------------------" << std::endl;

    std::cout << s.to_String(true) << std::endl;

    return 0;
}
