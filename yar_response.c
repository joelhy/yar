/*
  +----------------------------------------------------------------------+
  | Yar - Light, concurrent RPC framework                                |
  +----------------------------------------------------------------------+
  | Copyright (c) 2012-2013 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author:  Xinchen Hui   <laruence@php.net>                            |
  |          Zhenyu  Zhang <zhangzhenyu@php.net>                         |
  +----------------------------------------------------------------------+
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_yar.h"
#include "yar_exception.h"
#include "yar_request.h"
#include "yar_response.h"
#include "zend_exceptions.h"
#include "zend_smart_str.h"

yar_response_t * php_yar_response_instance() /* {{{ */ {
	yar_response_t *response = ecalloc(1, sizeof(yar_response_t));
	return response;
} /* }}} */

int php_yar_response_bind_request(yar_response_t *response, yar_request_t *request) /* {{{ */ {
    response->id = request->id;
	return 1;
} /* }}} */

void php_yar_response_alter_body(yar_response_t *response, zend_string *body, int method) /* {{{ */ {
	response->out = body;
} /* }}} */

void php_yar_response_set_error(yar_response_t *response, int type, char *message, uint len) /* {{{ */ {
	ZVAL_STRINGL(&response->err, message, len);
	response->status = type;
} /* }}} */

static inline zend_class_entry *i_get_exception_base(zval *object)
{
	return instanceof_function(Z_OBJCE_P(object), zend_ce_exception) ? zend_ce_exception : zend_ce_error;
}

#define TRACE_APPEND_KEY(key) do {                                          \
		tmp = zend_hash_str_find(ht, key, sizeof(key)-1);                   \
		if (tmp) {                                                          \
			if (Z_TYPE_P(tmp) != IS_STRING) {                               \
				zend_error(E_WARNING, "Value for %s is no string", key);    \
				smart_str_appends(str, "[unknown]");                        \
			} else {                                                        \
				smart_str_appends(str, Z_STRVAL_P(tmp));   \
			}                                                               \
		} \
	} while (0)

/* Windows uses VK_ESCAPE instead of \e */
#ifndef VK_ESCAPE
#define VK_ESCAPE '\e'
#endif

static size_t compute_escaped_string_len(const char *s, size_t l) {
	size_t i, len = l;
	for (i = 0; i < l; ++i) {
		char c = s[i];
		if (c == '\n' || c == '\r' || c == '\t' ||
			c == '\f' || c == '\v' || c == '\\' || c == VK_ESCAPE) {
			len += 1;
		} else if (c < 32 || c > 126) {
			len += 3;
		}
	}
	return len;
}

static void smart_str_append_escaped(smart_str *str, const char *s, size_t l) {
	char *res;
	size_t i, len = compute_escaped_string_len(s, l);

	smart_str_alloc(str, len, 0);
	res = &ZSTR_VAL(str->s)[ZSTR_LEN(str->s)];
	ZSTR_LEN(str->s) += len;

	for (i = 0; i < l; ++i) {
		unsigned char c = s[i];
		if (c < 32 || c == '\\' || c > 126) {
			*res++ = '\\';
			switch (c) {
				case '\n': *res++ = 'n'; break;
				case '\r': *res++ = 'r'; break;
				case '\t': *res++ = 't'; break;
				case '\f': *res++ = 'f'; break;
				case '\v': *res++ = 'v'; break;
				case '\\': *res++ = '\\'; break;
				case VK_ESCAPE: *res++ = 'e'; break;
				default:
					*res++ = 'x';
					if ((c >> 4) < 10) {
						*res++ = (c >> 4) + '0';
					} else {
						*res++ = (c >> 4) + 'A' - 10;
					}
					if ((c & 0xf) < 10) {
						*res++ = (c & 0xf) + '0';
					} else {
						*res++ = (c & 0xf) + 'A' - 10;
					}
			}
		} else {
			*res++ = c;
		}
	}
}

static void _build_trace_args(zval *arg, smart_str *str) /* {{{ */
{
	/* the trivial way would be to do
	 * convert_to_string_ex(arg);
	 * append it and kill the now tmp arg.
	 * but that could cause some E_NOTICE and also damn long lines.
	 */

	ZVAL_DEREF(arg);
	switch (Z_TYPE_P(arg)) {
		case IS_NULL:
			smart_str_appends(str, "NULL, ");
			break;
		case IS_STRING:
			smart_str_appendc(str, '\'');
			smart_str_append_escaped(str, Z_STRVAL_P(arg), MIN(Z_STRLEN_P(arg), 15));
			if (Z_STRLEN_P(arg) > 15) {
				smart_str_appends(str, "...', ");
			} else {
				smart_str_appends(str, "', ");
			}
			break;
		case IS_FALSE:
			smart_str_appends(str, "false, ");
			break;
		case IS_TRUE:
			smart_str_appends(str, "true, ");
			break;
		case IS_RESOURCE:
			smart_str_appends(str, "Resource id #");
			smart_str_append_long(str, Z_RES_HANDLE_P(arg));
			smart_str_appends(str, ", ");
			break;
		case IS_LONG:
			smart_str_append_long(str, Z_LVAL_P(arg));
			smart_str_appends(str, ", ");
			break;
		case IS_DOUBLE: {
			double dval = Z_DVAL_P(arg);
			char *s_tmp = emalloc(MAX_LENGTH_OF_DOUBLE + EG(precision) + 1);
			int l_tmp = zend_sprintf(s_tmp, "%.*G", (int) EG(precision), dval);  /* SAFE */
			smart_str_appendl(str, s_tmp, l_tmp);
			smart_str_appends(str, ", ");
			efree(s_tmp);
			break;
		}
		case IS_ARRAY:
			smart_str_appends(str, "Array, ");
			break;
		case IS_OBJECT:
			smart_str_appends(str, "Object(");
			smart_str_appends(str, ZSTR_VAL(Z_OBJCE_P(arg)->name));
			smart_str_appends(str, "), ");
			break;
	}
}
/* }}} */

static void _build_trace_string(smart_str *str, HashTable *ht, uint32_t num) /* {{{ */
{
	zval *file, *tmp;

	smart_str_appendc(str, '#');
	smart_str_append_long(str, num);
	smart_str_appendc(str, ' ');

	file = zend_hash_str_find(ht, "file", sizeof("file")-1);
	if (file) {
		if (Z_TYPE_P(file) != IS_STRING) {
			zend_error(E_WARNING, "Function name is no string");
			smart_str_appends(str, "[unknown function]");
		} else{
			zend_long line;
			tmp = zend_hash_str_find(ht, "line", sizeof("line")-1);
			if (tmp) {
				if (Z_TYPE_P(tmp) == IS_LONG) {
					line = Z_LVAL_P(tmp);
				} else {
					zend_error(E_WARNING, "Line is no long");
					line = 0;
				}
			} else {
				line = 0;
			}
			smart_str_append(str, Z_STR_P(file));
			smart_str_appendc(str, '(');
			smart_str_append_long(str, line);
			smart_str_appends(str, "): ");
		}
	} else {
		smart_str_appends(str, "[internal function]: ");
	}
	TRACE_APPEND_KEY("class");
	TRACE_APPEND_KEY("type");
	TRACE_APPEND_KEY("function");
	smart_str_appendc(str, '(');
	tmp = zend_hash_str_find(ht, "args", sizeof("args")-1);
	if (tmp) {
		if (Z_TYPE_P(tmp) == IS_ARRAY) {
			size_t last_len = ZSTR_LEN(str->s);
			zval *arg;

			ZEND_HASH_FOREACH_VAL(Z_ARRVAL_P(tmp), arg) {
				_build_trace_args(arg, str);
			} ZEND_HASH_FOREACH_END();

			if (last_len != ZSTR_LEN(str->s)) {
				ZSTR_LEN(str->s) -= 2; /* remove last ', ' */
			}
		} else {
			zend_error(E_WARNING, "args element is no array");
		}
	}
	smart_str_appends(str, ")\n");
}
/* }}} */

void php_yar_response_set_exception(yar_response_t *response, zend_object *ex) /* {{{ */ {
	zval *msg, *code, *file, *line, *trace,  *frame;
	zend_class_entry *ce;
	zval zv, rv, tmp;
	smart_str str = {0};
	uint32_t num = 0;

	ZVAL_OBJ(&zv, ex);
	ce = Z_OBJCE(zv);

	// get trace string begin
    trace = zend_read_property(ce, &zv, "trace", sizeof("trace")-1, 1, &rv);
    if (Z_TYPE_P(trace) == IS_ARRAY) {
        ZEND_HASH_FOREACH_VAL(Z_ARRVAL_P(trace),  frame) {
            if (Z_TYPE_P(frame) != IS_ARRAY) {
                zend_error(E_WARNING, "Expected array for frame %pu", 0);
                continue;
            }

            _build_trace_string(&str, Z_ARRVAL_P(frame), num++);
        } ZEND_HASH_FOREACH_END();

        smart_str_appendc(&str, '#');
        smart_str_append_long(&str, num);
        smart_str_appends(&str, " {main}");
        smart_str_0(&str);
    }
    // get trace string end

	msg = zend_read_property(ce, &zv, ZEND_STRL("message"), 0, &rv);
	code = zend_read_property(ce, &zv, ZEND_STRL("code"), 0, &rv);
	file = zend_read_property(ce, &zv, ZEND_STRL("file"), 0, &rv);
	line = zend_read_property(ce, &zv, ZEND_STRL("line"), 0, &rv);

	array_init(&response->err);

	Z_TRY_ADDREF_P(msg);
	Z_TRY_ADDREF_P(code);
	Z_TRY_ADDREF_P(file);
	Z_TRY_ADDREF_P(line);

	add_assoc_zval_ex(&response->err, ZEND_STRL("message"), msg);
	add_assoc_zval_ex(&response->err, ZEND_STRL("code"), code);
	add_assoc_zval_ex(&response->err, ZEND_STRL("file"), file);
	add_assoc_zval_ex(&response->err, ZEND_STRL("line"), line);

	add_assoc_str_ex(&response->err, ZEND_STRL("_type"), ce->name);
	add_assoc_str_ex(&response->err, ZEND_STRL("_server_trace_string"), str.s);

	response->status = YAR_ERR_EXCEPTION;
} /* }}} */

void php_yar_response_set_retval(yar_response_t *response, zval *retval) /* {{{ */ {
	ZVAL_COPY(&response->retval, retval);
} /* }}} */

void php_yar_response_map_retval(yar_response_t *response, zval *ret) /* {{{ */ {
	if (IS_ARRAY != Z_TYPE_P(ret)) {         
		return;
	} else { 
		zval *pzval;                       
		HashTable *ht = Z_ARRVAL_P(ret);     

		if ((pzval = zend_hash_str_find(ht, ZEND_STRL("i"))) == NULL) {
			return;
		}                                    
		convert_to_long(pzval);            
		response->id = Z_LVAL_P(pzval);    

		if ((pzval = zend_hash_str_find(ht, ZEND_STRL("s"))) == NULL) {
			return;
		}                                    
		convert_to_long(pzval);            
		if ((response->status = Z_LVAL_P(pzval)) == YAR_ERR_OKEY) {
			if ((pzval = zend_hash_str_find(ht, ZEND_STRL("o"))) != NULL) {
				response->out = Z_STR_P(pzval);
				ZVAL_NULL(pzval);          
			}                                
			if ((pzval = zend_hash_str_find(ht, ZEND_STRL("r"))) != NULL) {
				ZVAL_COPY(&response->retval, pzval);
			}                                
		} else if ((pzval = zend_hash_str_find(ht, ZEND_STRL("e"))) != NULL) {
			ZVAL_COPY(&response->err, pzval);
		}                      
	}
}
/* }}} */

void php_yar_response_destroy(yar_response_t *response) /* {{{ */ {
	if (response->out) {
		zend_string_release(response->out);
	}

	zval_ptr_dtor(&response->retval);
	zval_ptr_dtor(&response->err);

	efree(response);
} /* }}} */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 * vim600: noet sw=4 ts=4 fdm=marker
 * vim<600: noet sw=4 ts=4
 */
