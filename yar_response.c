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

/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_yar.h"
#include "yar_exception.h"
#include "yar_request.h"
#include "yar_response.h"
#include "zend_hash.h"
#include "zend_API.h"
#include "zend_exceptions.h"

yar_response_t * php_yar_response_instance(TSRMLS_D) /* {{{ */ {
	yar_response_t *response = ecalloc(sizeof(yar_response_t), 1);

	return response;
} /* }}} */

int php_yar_response_bind_request(yar_response_t *response, yar_request_t *request TSRMLS_DC) /* {{{ */ {
    response->id = request->id;
	return 1;
} /* }}} */

void php_yar_response_alter_body(yar_response_t *response, char *body, uint len, int method TSRMLS_DC) /* {{{ */ {
	response->out = body;
	response->olen = len;
} /* }}} */

void php_yar_response_set_error(yar_response_t *response, int type, char *message, uint len TSRMLS_DC) /* {{{ */ {
	zval *msg;
	response->status = type;
	MAKE_STD_ZVAL(msg);
	ZVAL_STRINGL(msg, message, len, 1);
	response->err = msg;
} /* }}} */

/* {{{ gettraceasstring() macros */
#define TRACE_APPEND_CHR(chr)                                            \
	*str = (char*)erealloc(*str, *len + 1 + 1);                          \
	(*str)[(*len)++] = chr

#define TRACE_APPEND_STRL(val, vallen)                                   \
	{                                                                    \
		int l = vallen;                                                  \
		*str = (char*)erealloc(*str, *len + l + 1);                      \
		memcpy((*str) + *len, val, l);                                   \
		*len += l;                                                       \
	}

#define TRACE_APPEND_STR(val)                                            \
	TRACE_APPEND_STRL(val, sizeof(val)-1)

#define TRACE_APPEND_KEY(key)                                                   \
	if (zend_hash_find(ht, key, sizeof(key), (void**)&tmp) == SUCCESS) {    \
		if (Z_TYPE_PP(tmp) != IS_STRING) {                              \
			zend_error(E_WARNING, "Value for %s is no string", key); \
			TRACE_APPEND_STR("[unknown]");                          \
		} else {                                                        \
			TRACE_APPEND_STRL(Z_STRVAL_PP(tmp), Z_STRLEN_PP(tmp));  \
		}                                                               \
	}

#define TRACE_ARG_APPEND(vallen)								\
	*str = (char*)erealloc(*str, *len + 1 + vallen);					\
	memmove((*str) + *len - l_added + 1 + vallen, (*str) + *len - l_added + 1, l_added);

/* }}} */

static int _build_trace_args(zval **arg TSRMLS_DC, int num_args, va_list args, zend_hash_key *hash_key) /* {{{ */
{
    char **str;
    int *len;

    str = va_arg(args, char**);
    len = va_arg(args, int*);

    /* the trivial way would be to do:
     * convert_to_string_ex(arg);
     * append it and kill the now tmp arg.
     * but that could cause some E_NOTICE and also damn long lines.
     */

    switch (Z_TYPE_PP(arg)) {
        case IS_NULL:
        TRACE_APPEND_STR("NULL, ");
            break;
        case IS_STRING: {
            int l_added;
            TRACE_APPEND_CHR('\'');
            if (Z_STRLEN_PP(arg) > 15) {
                TRACE_APPEND_STRL(Z_STRVAL_PP(arg), 15);
                TRACE_APPEND_STR("...', ");
                l_added = 15 + 6 + 1; /* +1 because of while (--l_added) */
            } else {
                l_added = Z_STRLEN_PP(arg);
                TRACE_APPEND_STRL(Z_STRVAL_PP(arg), l_added);
                TRACE_APPEND_STR("', ");
                l_added += 3 + 1;
            }
            while (--l_added) {
                unsigned char chr = (*str)[*len - l_added];
                if (chr < 32 || chr == '\\' || chr > 126) {
                    (*str)[*len - l_added] = '\\';

                    switch (chr) {
                        case '\n':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 'n';
                            break;
                        case '\r':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 'r';
                            break;
                        case '\t':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 't';
                            break;
                        case '\f':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 'f';
                            break;
                        case '\v':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 'v';
                            break;
#ifndef PHP_WIN32
                        case '\e':
#else
                            case VK_ESCAPE:
#endif
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = 'e';
                            break;
                        case '\\':
                            TRACE_ARG_APPEND(1);
                            (*str)[++(*len) - l_added] = '\\';
                            break;
                        default:
                            TRACE_ARG_APPEND(3);
                            (*str)[*len - l_added + 1] = 'x';
                            if ((chr >> 4) < 10) {
                                (*str)[*len - l_added + 2] = (chr >> 4) + '0';
                            } else {
                                (*str)[*len - l_added + 2] = (chr >> 4) + 'A' - 10;
                            }
                            if (chr % 16 < 10) {
                                (*str)[*len - l_added + 3] = chr % 16 + '0';
                            } else {
                                (*str)[*len - l_added + 3] = chr % 16 + 'A' - 10;
                            }
                            *len += 3;
                    }
                }
            }
            break;
        }
        case IS_BOOL:
            if (Z_LVAL_PP(arg)) {
                TRACE_APPEND_STR("true, ");
            } else {
                TRACE_APPEND_STR("false, ");
            }
            break;
        case IS_RESOURCE:
        TRACE_APPEND_STR("Resource id #");
            /* break; */
        case IS_LONG: {
            long lval = Z_LVAL_PP(arg);
            char s_tmp[MAX_LENGTH_OF_LONG + 1];
            int l_tmp = zend_sprintf(s_tmp, "%ld", lval);  /* SAFE */
            TRACE_APPEND_STRL(s_tmp, l_tmp);
            TRACE_APPEND_STR(", ");
            break;
        }
        case IS_DOUBLE: {
            double dval = Z_DVAL_PP(arg);
            char *s_tmp;
            int l_tmp;

            s_tmp = emalloc(MAX_LENGTH_OF_DOUBLE + EG(precision) + 1);
            l_tmp = zend_sprintf(s_tmp, "%.*G", (int) EG(precision), dval);  /* SAFE */
            TRACE_APPEND_STRL(s_tmp, l_tmp);
            /* %G already handles removing trailing zeros from the fractional part, yay */
            efree(s_tmp);
            TRACE_APPEND_STR(", ");
            break;
        }
        case IS_ARRAY:
        TRACE_APPEND_STR("Array, ");
            break;
        case IS_OBJECT: {
            const char *class_name;
            zend_uint class_name_len;
            int dup;

            TRACE_APPEND_STR("Object(");

            dup = zend_get_object_classname(*arg, &class_name, &class_name_len TSRMLS_CC);

            TRACE_APPEND_STRL(class_name, class_name_len);
            if(!dup) {
                efree((char*)class_name);
            }

            TRACE_APPEND_STR("), ");
            break;
        }
        default:
            break;
    }
    return ZEND_HASH_APPLY_KEEP;
}
/* }}} */

static int _build_trace_string(zval **frame TSRMLS_DC, int num_args, va_list args, zend_hash_key *hash_key) /* {{{ */
{
    char *s_tmp, **str;
    int *len, *num;
    long line;
    HashTable *ht = Z_ARRVAL_PP(frame);
    zval **file, **tmp;

    if (Z_TYPE_PP(frame) != IS_ARRAY) {
        zend_error(E_WARNING, "Expected array for frame %lu", hash_key->h);
        return ZEND_HASH_APPLY_KEEP;
    }

    str = va_arg(args, char**);
    len = va_arg(args, int*);
    num = va_arg(args, int*);

    s_tmp = emalloc(1 + MAX_LENGTH_OF_LONG + 1 + 1);
    sprintf(s_tmp, "#%d ", (*num)++);
    TRACE_APPEND_STRL(s_tmp, strlen(s_tmp));
    efree(s_tmp);
    if (zend_hash_find(ht, "file", sizeof("file"), (void**)&file) == SUCCESS) {
        if (Z_TYPE_PP(file) != IS_STRING) {
            zend_error(E_WARNING, "Function name is no string");
            TRACE_APPEND_STR("[unknown function]");
        } else{
            if (zend_hash_find(ht, "line", sizeof("line"), (void**)&tmp) == SUCCESS) {
                if (Z_TYPE_PP(tmp) == IS_LONG) {
                    line = Z_LVAL_PP(tmp);
                } else {
                    zend_error(E_WARNING, "Line is no long");
                    line = 0;
                }
            } else {
                line = 0;
            }
            s_tmp = emalloc(Z_STRLEN_PP(file) + MAX_LENGTH_OF_LONG + 4 + 1);
            sprintf(s_tmp, "%s(%ld): ", Z_STRVAL_PP(file), line);
            TRACE_APPEND_STRL(s_tmp, strlen(s_tmp));
            efree(s_tmp);
        }
    } else {
        TRACE_APPEND_STR("[internal function]: ");
    }
    TRACE_APPEND_KEY("class");
    TRACE_APPEND_KEY("type");
    TRACE_APPEND_KEY("function");
    TRACE_APPEND_CHR('(');
    if (zend_hash_find(ht, "args", sizeof("args"), (void**)&tmp) == SUCCESS) {
        if (Z_TYPE_PP(tmp) == IS_ARRAY) {
            int last_len = *len;
            zend_hash_apply_with_arguments(Z_ARRVAL_PP(tmp) TSRMLS_CC, (apply_func_args_t)_build_trace_args, 2, str, len);
            if (last_len != *len) {
                *len -= 2; /* remove last ', ' */
            }
        } else {
            zend_error(E_WARNING, "args element is no array");
        }
    }
    TRACE_APPEND_STR(")\n");
    return ZEND_HASH_APPLY_KEEP;
}
/* }}} */

void php_yar_response_set_exception(yar_response_t *response, zval *ex TSRMLS_DC) /* {{{ */ {
	zval *msg, *code, *file, *line, *ret, *trace;
	zend_class_entry *ce;
	char *res, **str, *s_tmp;
	int res_len = 0, *len = &res_len, num = 0;

	ce = Z_OBJCE_P(ex);

	trace = zend_read_property(ce, ex, "trace", sizeof("trace")-1, 1 TSRMLS_CC);
	if(Z_TYPE_P(trace) == IS_ARRAY) {
		res = estrdup("");
		str = &res;

		zend_hash_apply_with_arguments(Z_ARRVAL_P(trace) TSRMLS_CC, (apply_func_args_t)_build_trace_string, 3, str, len, &num);

		s_tmp = emalloc(1 + MAX_LENGTH_OF_LONG + 7 + 1);
		sprintf(s_tmp, "#%d {main}", num);
		TRACE_APPEND_STRL(s_tmp, strlen(s_tmp));
		efree(s_tmp);

		res[res_len] = '\0';
	}

	msg = zend_read_property(ce, ex, ZEND_STRL("message"), 0 TSRMLS_CC);
	code = zend_read_property(ce, ex, ZEND_STRL("code"), 0 TSRMLS_CC);
	file = zend_read_property(ce, ex, ZEND_STRL("file"), 0 TSRMLS_CC);
	line = zend_read_property(ce, ex, ZEND_STRL("line"), 0 TSRMLS_CC);

	MAKE_STD_ZVAL(ret);
	array_init(ret);

	Z_ADDREF_P(msg);
	Z_ADDREF_P(code);
	Z_ADDREF_P(file);
	Z_ADDREF_P(line);

	add_assoc_zval_ex(ret, ZEND_STRS("message"), msg);
	add_assoc_zval_ex(ret, ZEND_STRS("code"), code);
	add_assoc_zval_ex(ret, ZEND_STRS("file"), file);
	add_assoc_zval_ex(ret, ZEND_STRS("line"), line);

	add_assoc_string_ex(ret, ZEND_STRS("_type"), (char *)ce->name, 1);
	add_assoc_string_ex(ret, ZEND_STRS("_server_trace_string"), res, 1);

	response->status = YAR_ERR_EXCEPTION;
    response->err = ret;
	zval_ptr_dtor(&ex);
} /* }}} */

void php_yar_response_set_retval(yar_response_t *response, zval *retval TSRMLS_DC) /* {{{ */ {
	response->retval = retval;
} /* }}} */

void php_yar_response_map_retval(yar_response_t *response, zval *ret TSRMLS_DC) /* {{{ */ {
	if (IS_ARRAY != Z_TYPE_P(ret)) {         
		return;
	} else { 
		zval **ppzval;                       
		HashTable *ht = Z_ARRVAL_P(ret);     

		if (zend_hash_find(ht, ZEND_STRS("i"), (void **)&ppzval) == FAILURE) {
			return;
		}                                    
		convert_to_long(*ppzval);            
		response->id = Z_LVAL_PP(ppzval);    

		if (zend_hash_find(ht, ZEND_STRS("s"), (void **)&ppzval) == FAILURE) {
			return;
		}                                    
		convert_to_long(*ppzval);            
		if ((response->status = Z_LVAL_PP(ppzval)) == YAR_ERR_OKEY) {
			if (zend_hash_find(ht, ZEND_STRS("o"), (void **)&ppzval) == SUCCESS) {
				response->out = Z_STRVAL_PP(ppzval);
				response->olen = Z_STRLEN_PP(ppzval);
				ZVAL_NULL(*ppzval);          
			}                                
			if (zend_hash_find(ht, ZEND_STRS("r"), (void **)&ppzval) == SUCCESS) {
				Z_ADDREF_P(*ppzval);         
				response->retval = *ppzval;  
			}                                
		} else if (zend_hash_find(ht, ZEND_STRS("e"), (void **)&ppzval) == SUCCESS) {
			Z_ADDREF_P(*ppzval);
			response->err = *ppzval;
		}                      
	}
}
/* }}} */

void php_yar_response_destroy(yar_response_t *response TSRMLS_DC) /* {{{ */ {
	if (response->out) {
		efree(response->out);
	}

	if (response->retval) {
		zval_ptr_dtor(&response->retval);
	}

	if (response->err) {
		zval_ptr_dtor(&response->err);
	}

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
