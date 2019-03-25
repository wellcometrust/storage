# -*- encoding: utf-8

import functools

from oauthlib.oauth2 import BackendApplicationClient
from requests_oauthlib import OAuth2Session

from .exceptions import IngestNotFound, ServerError, UserError


def check_api_resp(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        resp = f(*args, **kwargs)
        if resp.status_code >= 500:
            raise ServerError("Unexpected error from storage service")
        elif resp.status_code >= 400:
            raise UserError(
                "Storage service reported a user error: %s" %
                resp.json()["description"]
            )
        else:
            return resp.json()

    return wrapper


class StorageServiceClient:
    """
    Client for the Wellcome Storage Service API.
    """

    def __init__(self, api_url, sess):
        self.api_url = api_url
        self.sess = sess

    @classmethod
    def with_oauth(self, api_url, client_id, token_url, client_secret):
        client = BackendApplicationClient(client_id=client_id)
        sess = OAuth2Session(client=client)
        sess.fetch_token(
            token_url=token_url,
            client_id=client_id,
            client_secret=client_secret
        )
        return StorageServiceClient(api_url=api_url, sess=sess)

    @check_api_resp
    def get_space(self, space_id):
        """
        Get information about a named space.
        """
        # This isn't implemented in the storage service yet, so we can't
        # write a client handler for it!
        raise NotImplementedError

    @check_api_resp
    def get_ingest(self, ingest_id):
        """
        Query the state of an individual ingest.
        """
        ingests_api_url = self.api_url + "/ingests/%s" % ingest_id
        resp = self.sess.get(ingests_api_url)

        if resp.status_code == 404:
            raise IngestNotFound("Ingests API returned 404 for ingest %s" % ingest_id)
        else:
            return resp
