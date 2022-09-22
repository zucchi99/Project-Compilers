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

        ~Value() {}

        ContentType type() {
            return m_ctType;
        }

        std::string stringValue() { return m_strContent; }
        int integerValue() { return m_iContent; }
        bool boolValue() { return m_bContent; }
        std::pair<std::string, Value*> pointerValue() { return m_pContent; }

        void setPointerValue(Value* p_value) { m_pContent.second = p_value; }

        // Restistuisce una stringa contenente il pretty_printer della Configurazione
        std::string to_String(){
            std::string string_content = "";

            switch(m_ctType) {
                case Value::String: string_content = m_strContent; break;
                case Value::Integer: string_content = std::to_string(m_iContent); break;
                case Value::Bool: string_content = m_bContent ? "true" : "false"; break;
                case Value::Pointer: string_content = m_pContent.first; break;
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



int test() {
    std::map<int, Value> mapAnyValue;

    mapAnyValue[0] = Value(1337);
    mapAnyValue[1] = Value("Collegamento2", &mapAnyValue[0]);
    mapAnyValue[2] = Value("Collegamento2", &mapAnyValue[1]);
    //mapAnyValue[2] = Value("Collegamento1", &mapAnyValue[2]);

    std::cout << mapAnyValue[2] << std::endl;

    return 0;
}