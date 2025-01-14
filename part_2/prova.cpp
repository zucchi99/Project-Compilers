#include <iostream>
#include "Configuration.hpp"

#define my_pair std::pair<Value, std::list<Value*>>

int main() {
    Configuration s;
    std::list<Value*> l;
    
    // -------------------------------------------------------------------------------

    std::list<std::pair<std::string, my_pair>> assignments_s1 = {
        { "a", my_pair(Value("ciao"), l) }, 
        { "b", my_pair(Value(1),      l) }, 
        { "c", my_pair(Value(true),   l) }
    };
    s.insert_empty_section("sezione1");
    s.modify_assignments_to_section("sezione1", assignments_s1);

    // -------------------------------------------------------------------------------

    std::list<std::pair<std::string, my_pair>> assignments_s2 = {
        { "d", my_pair(Value("$sezione1.a", NULL), l) }, 
        { "e", my_pair(Value("$sezione2.d", NULL), l) }, 
        { "f", my_pair(Value("$sezione3.g", NULL), l) }, 
    };
    s.insert_empty_section("sezione2");
    s.modify_assignments_to_section("sezione2", assignments_s2);
    
    // -------------------------------------------------------------------------------
    
    std::list<std::pair<std::string, my_pair>> assignments_s3 = {
        { "g", my_pair(Value("$sezione2.e", NULL), l) }, 
        { "h", my_pair(Value("$sezione2.f", NULL), l) }, 
        { "i", my_pair(Value("$sezione3.h", NULL), l) }, 
    };
    s.insert_empty_section("sezione3");
    s.modify_assignments_to_section("sezione3", assignments_s3);
    
    // -------------------------------------------------------------------------------

    std::list<std::pair<std::string, my_pair>> assignments_s4 = {
        { "a", my_pair(Value("old"), l) }, 
        { "b", my_pair(Value("new"), l) },  
        { "x", my_pair(Value("$a", NULL), l) }
    };
    s.insert_empty_section("sezione4");
    s.modify_assignments_to_section("sezione4", assignments_s4);

    std::list<std::pair<std::string, my_pair>> assignments_s4_edit = { { "x", my_pair(Value("$b", NULL), l) } };

    s.modify_assignments_to_section("sezione4", assignments_s4_edit);
    // -------------------------------------------------------------------------------

    s.add_pointers_references();

    std::cout << s << std::endl;
    //std::cout << s.to_String(true) << std::endl;

    std::cout << "--------------------------------------------------" << std::endl;


    std::cout << "s.delete_section(\"sezione2\")" << std::endl;
    s.delete_section("sezione2");  

    std::cout << s << std::endl;
    //std::cout << s.to_String(true) << std::endl;

    std::cout << "--------------------------------------------------" << std::endl;

    s.get_backlist("sezione4", "a");
    
    std::cout << "--------------------------------------------------" << std::endl;

    s.get_backlist("sezione4", "b");

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
    auto assignment7 = my_pair(Value("$e", NULL), l);

       std::list<std::pair<std::string, my_pair>> assignments = {
        { "a", assignment1 }, 
        { "b", assignment2 }, 
        { "c", assignment3 }, 
        { "d", assignment4 }, 
        { "e", assignment6 }, 
        { "f", assignment7 }
    };
    
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



