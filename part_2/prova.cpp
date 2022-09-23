#include <iostream>
#include "Configuration.hpp"

int main() {
    Configuration s;
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

    // std::cout << s << std::endl;

    // s.delete_section("sezione1");

    std::cout << s << std::endl;

    // // Caso1
    // s.delete_assignment("sezione1", "d");
    // std::cout << "Caso 1, sezione1 d " << std::endl << s << std::endl;
    // // Caso2
    //s.delete_assignment("sezione1", "a");
    //std::cout << "Caso 2, sezione1 a " << std::endl << s << std::endl;
    // Caso3
    // s.delete_assignment("sezione1", "e");
    // std::cout << "Caso 3, sezione1 e " << std::endl << s << std::endl;
    // // Caso4
    // s.delete_assignment("sezione1", "b");
    // std::cout << "Caso 4, sezione1 b " << std::endl << s << std::endl;

    std::cout << s.to_String(true) << std::endl;

    return 0;
}
