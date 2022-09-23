#include <iostream>
#include <memory>
#include <map>
#include <string>

class Value {
    public:
        typedef enum {
            String,
            Integer,
            Bool,
            Pointer
        } ContentType;

    private:
        ContentType m_ctType;

        std::string m_strContent;
        int m_iContent;
        bool m_bContent;
        std::pair<std::string, Value*> m_pContent = std::pair<std::string, Value*>("", NULL);

    public:
        Value() : m_strContent(""), m_ctType(String) {}
        explicit Value(const char* arrcString) : m_strContent(std::string(arrcString)), m_ctType(String) {}
        explicit Value(std::string strContent) : m_strContent(strContent), m_ctType(String) {}
        explicit Value(int iContent) : m_iContent(iContent), m_ctType(Integer) {}
        explicit Value(bool bContent) : m_bContent(bContent), m_ctType(Bool) {}
        explicit Value(std::string ref, Value* valuePointer) : m_pContent(ref, valuePointer), m_ctType(Pointer) {}
        explicit Value(const Value& value) : m_ctType(value.m_ctType), m_strContent(value.m_strContent), m_iContent(value.m_iContent), m_bContent(value.m_bContent), m_pContent(value.m_pContent.first, value.m_pContent.second) {}

        ~Value() {}

        ContentType type() {
            return m_ctType;
        }

        std::string stringValue() { return m_strContent; }
        int integerValue() { return m_iContent; }
        bool boolValue() { return m_bContent; }
        std::pair<std::string, Value*> pointerValue() { return m_pContent; }

        std::pair<std::string, std::string> pointerAssignment() {
            auto p_name = m_pContent.first;

            // get section name pointed
            int dot_idx = p_name.find('.');
            std::string sezione = (dot_idx == std::string::npos) ? "" : p_name.substr(1,dot_idx-1);

            // get variable name pointed
            if(dot_idx != std::string::npos) {
                // remove section name pointed
                p_name = p_name.substr(dot_idx+1);
            } else {
                // remove starting $
                p_name = p_name.substr(1);
            }

            std::pair<std::string, std::string> coppia = std::pair<std::string, std::string>(sezione, p_name);

            return coppia;
        }

        void setPointerValue(Value* p_value) { m_pContent.second = p_value; }
        void setPointerValue(std::string label, Value* p_value) { 
            m_pContent.first = label;
            m_pContent.second = p_value; 
        }

        // Restistuisce una stringa contenente il pretty_printer della Configurazione
        // direct_value, nel caso l'instanza dell'oggetto sia di tipo "Pointer", se è "true" ritornerà i valori delle variabili puntate, mentre con "false" verranno restituiti i riferimenti.
        std::string to_String(bool direct_value){
            std::string string_content = "";

            switch(m_ctType) {
                case Value::String: string_content = m_strContent; break;
                case Value::Integer: string_content = std::to_string(m_iContent); break;
                case Value::Bool: string_content = m_bContent ? "true" : "false"; break;
                case Value::Pointer: 
                    if (direct_value==true){
                        string_content = (*m_pContent.second).to_String(direct_value);
                    }else{
                        string_content = m_pContent.first;
                    }
                    break;
            }

            return string_content;
        }
};

std::ostream& operator<<(std::ostream& osStream, Value& valOut) {
    switch(valOut.type()) {
        case Value::String: osStream << valOut.stringValue(); break;
        case Value::Integer: osStream << valOut.integerValue(); break;
        case Value::Bool: osStream << valOut.boolValue(); break;
        case Value::Pointer: osStream << valOut.pointerValue().first << " refers to -> " << *valOut.pointerValue().second;  break;
    }

    return osStream;
}