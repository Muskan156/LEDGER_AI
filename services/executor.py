import importlib.util
import sys

def load_parser_module(file_path):
    spec = importlib.util.spec_from_file_location("generated_parser", file_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules["generated_parser"] = module
    spec.loader.exec_module(module)
    return module

def execute_parser(pdf_path, parser_path, password=None):
    parser_module = load_parser_module(parser_path)
    transactions = parser_module.parse_pdf(pdf_path, password)
    return transactions
